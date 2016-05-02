/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.transformer.ClawTransformer;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A Kcaching transformation is an independent transformation. The
 * transformation consists of placing an assignment in a scalar variable and
 * use this variable in a loop body before updating it.
 *
 * @author clementval
 */
public class Kcaching extends Transformation {
  private final ClawLanguage _claw;
  private XdoStatement _doStmt;

  /**
   * Constructs a new Kcachine triggered from a specific pragma.
   * @param directive  The directive that triggered the k caching transformation.
   */
  public Kcaching(ClawLanguage directive) {
    super(directive);
    _claw = directive;
  }

  /**
   * @see Transformation#analyze(XcodeProgram, Transformer)
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    _doStmt = XelementHelper.findParentDoStmt(_claw.getPragma());
    if(_doStmt == null){
      xcodeml.addError("The kcache directive is not nested in a do statement",
          _claw.getPragma().getLineNo());
      return false;
    }

    return true;
  }

  /**
   * @see Transformation#canBeTransformedWith(Transformation)
   */
  @Override
  public boolean canBeTransformedWith(Transformation other) {
    // independant transformation
    return false;
  }

  /**
   * @see Transformation#transform(XcodeProgram, Transformer, Transformation)
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
    // It might have change from the analysis
    _doStmt = XelementHelper.findParentDoStmt(_claw.getPragma());

    // Check if there is an assignment

    // 1. Find the function/module declaration
    // TODO find parent definition with symbols and declaration table
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());

    for(String data : _claw.getDataClauseValues()){
      XassignStatement stmt =
          XelementHelper.getFirstArrayAssign(_claw.getPragma(), data);

      boolean standardArrayRef = true;
      if(stmt != null) {
        for (XbaseElement el :
            stmt.getLValueModel().getArrayRef().getInnerElements()) {
          if (el instanceof XarrayIndex) {
            if (!(((XarrayIndex) el).getExprModel().isVar() ||
                ((XarrayIndex) el).getExprModel().isConstant())) {
              standardArrayRef = false;
            }
          }
        }
      }

      if(stmt != null && standardArrayRef){
        transformAssignStmt(xcodeml, fctDef, data, stmt, transformer);
      } else {
        transformData(xcodeml, fctDef, data, transformer);
      }

    }
    _claw.getPragma().delete();
  }

  /**
   * Apply the tranformation for the data list.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param fctDef      Function/module definition in which the data are nested.
   * @param data        Array identifier on which the caching is done.
   * @param transformer Current instance of the transformer.
   * @throws Exception If smth prevent the transformation to be done.
   */
  private void transformData(XcodeProgram xcodeml, XfunctionDefinition fctDef,
                             String data,
                             Transformer transformer)
      throws Exception
  {

    List<XarrayRef> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, data);

    // Generate the cache variable and its assignment
    String type = aRefs.get(0).getType();
    Xvar cacheVar = generateCacheVarAndAssignStmt(xcodeml, data, type, fctDef,
        aRefs.get(0), null);

    updateArrayRefWithCache(aRefs, cacheVar);

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, transformer,
        _claw.getPragma(), cacheVar.getValue());
  }

  /**
   * Apply the transformation for the LHS array reference.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param fctDef       Function/module definition in which the data are nested.
   * @param data         Array identifier on which the caching is done.
   * @param stmt         First statement including the array ref on the lhs.
   * @param transformer  The transformer used to applied the transformations.
   * @throws Exception If smth prevent the transformation to be done.
   */
  private void transformAssignStmt(XcodeProgram xcodeml,
                                   XfunctionDefinition fctDef,
                                   String data,
                                   XassignStatement stmt,
                                   Transformer transformer) throws Exception
  {
    String type = stmt.getLValueModel().getArrayRef().getType();

    List<XarrayRef> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, data);

    Xvar cacheVar =
        generateCacheVarAndAssignStmt(xcodeml, data, type, fctDef, stmt, stmt);

    applyInitClause(xcodeml, transformer, cacheVar, aRefs.get(0));

    updateArrayRefWithCache(aRefs, cacheVar);

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, transformer,
        _claw.getPragma(), cacheVar.getValue());
    stmt.delete();
  }

  /**
   * Apply the init clause if it was part of the kcache directive.
   * @param xcodeml     Current program in which the transformation is
   *                    performed.
   * @param transformer Current transformer used to store elements informations.
   * @param cacheVar    Newly created cache variable that will be used for the
   *                    initialization (rhs of the assign statement). Element
   *                    will be cloned before insertion.
   * @param arrayRef    Array reference to be modified that will be
   *                    used for the initialization (lhs of the assign
   *                    statement). Element will be cloned before insertion.
   * @throws IllegalTransformationException
   */
  private void applyInitClause(XcodeProgram xcodeml, Transformer transformer,
                               Xvar cacheVar, XarrayRef arrayRef)
    throws IllegalTransformationException
  {

    if(_claw.hasInitClause()){
      ClawTransformer ct = (ClawTransformer)transformer;
      XifStatement initIfStmt = (XifStatement) ct.hasElement(_doStmt);
      if(initIfStmt == null){
        // If statement has not been created yet so we do it here
        initIfStmt = XifStatement.create(xcodeml);
        XelementHelper.copyEnhancedInfo(_claw.getPragma(), initIfStmt);
        XbinaryExpr logEq =
            XelementHelper.createEmpty(XelementName.LOG_EQ_EXPR, xcodeml);

        // Set lhs of equality
        logEq.appendToChildren(_doStmt.getIterationRange().getInductionVar(),
            true);
        // Set rhs of equality
        logEq.appendToChildren(_doStmt.getIterationRange().getIndexRange().
            getLowerBound().getExprModel().getElement(), true);

        initIfStmt.getCondition().appendToChildren(logEq, false);
        _doStmt.getBody().appendAsFirst(initIfStmt);
        ct.storeElement(_doStmt, initIfStmt);
      }

      XassignStatement initAssignement =
          XelementHelper.createEmpty(XassignStatement.class, xcodeml);
      initAssignement.appendToChildren(cacheVar, true); // set rhs
      initAssignement.appendToChildren(arrayRef, true); // set lhs
      // Add assignment in the "then" body element
      initIfStmt.getThen().getBody().appendToChildren(initAssignement, false);
    }
  }

  /**
   * Generate a new variable name with the offsets information.
   * @param basename The original variable name.
   * @param offsets  The offsets to be applied.
   * @return Original name with offsets information.
   */
  private String generateNameWithOffsetInfo(String basename,
                                            List<Integer> offsets)
  {
    String newName = basename + "_k";
    for(Integer i : offsets){
      if(i > 0) {
        newName += "p" + i;
      } else if (i < 0){
        newName += "m" + Math.abs(i);
      } else {
        newName += "_";
      }
    }
    return newName;
  }

  /**
   * Generate correct offset inferred by the dimension of the variable.
   * @param xcodeml The current program
   * @param fctDef  The function definition which holds the variable
   *                information.
   * @param var     The variable on which the offset are inferred.
   * @return List of integer representing the offset for the given variable.
   * @throws IllegalTransformationException if symbole id is not found.
   */
  private List<Integer> generateInferredOffsets(XcodeProgram xcodeml,
                                                XfunctionDefinition fctDef,
                                                String var)
      throws IllegalTransformationException
  {
    Xid id = fctDef.getSymbolTable().get(var);
    if(id == null){
      throw new IllegalTransformationException("Variable " + var +
          " defined in the data clause has not been found",
          _claw.getPragma().getLineNo()
      );
    }
    XbasicType btype = (XbasicType)xcodeml.getTypeTable().get(id.getType());
    int dim = btype.getDimensions();
    List<Integer> offsets = new ArrayList<>();
    for(int i = 0; i < dim; ++i){
      offsets.add(0);
    }
    return offsets;
  }

  /**
   * Generate the necessary intermediate code to create the new cache variable
   * and set its assignment.
   * @param xcodeml The current program.
   * @param var     The original variable name.
   * @param type    The original variable type.
   * @param fctDef  The function definition holding the variable.
   * @param rhs     The element that will be set as the rhs of the assignment.
   * @param stmt    The assign statement including the array ref.
   * @return The new created Xvar element.
   * @throws IllegalTransformationException
   */
  private Xvar generateCacheVarAndAssignStmt(XcodeProgram xcodeml, String var,
                                             String type,
                                             XfunctionDefinition fctDef,
                                             XbaseElement rhs,
                                             XassignStatement stmt)
      throws IllegalTransformationException
  {
    XbasicType t = (XbasicType) xcodeml.getTypeTable().get(type);
    if(t.getIntent() != null || t.isAllocatable()){
      // Type has an intent ... duplicate it and remove it
      XbasicType newType = t.cloneObject();
      type = xcodeml.getTypeTable().generateRealTypeHash();
      newType.setType(type);
      newType.removeIntent();
      newType.removeAllocatable();

      XbasicType ref = (XbasicType) xcodeml.getTypeTable().get(newType.getRef());
      if(ref != null && (ref.isAllocatable() || ref.hasIntent())){
        // TODO is there several level to reach ref ? Check if ref is Freal ...
        XbasicType newRef = ref.cloneObject();
        // TODO generate appropriate type
        String refType = xcodeml.getTypeTable().generateRealTypeHash();
        newRef.setType(refType);
        newRef.removeIntent();
        newRef.removeAllocatable();
        newType.setRef(refType);
        xcodeml.getTypeTable().add(newRef);
      }
      xcodeml.getTypeTable().add(newType);
    }


    String cacheName =
        generateNameWithOffsetInfo(var, _claw.getOffsets());

    // 2.2 inject a new entry in the symbol table
    if(!fctDef.getSymbolTable().contains(cacheName)){
      Xid cacheVarId = Xid.create(type, XelementName.SCLASS_F_LOCAL, cacheName,
          xcodeml);
      fctDef.getSymbolTable().add(cacheVarId, false);
    }

    // 2.3 inject a new entry in the declaration table
    if(!fctDef.getDeclarationTable().contains(cacheName)){
      XvarDecl cacheVarDecl = XvarDecl.create(type, cacheName, xcodeml);
      fctDef.getDeclarationTable().add(cacheVarDecl);
    }

    // 2.4 Prepare the new variable that is used for caching
    Xvar cacheVar = Xvar.create(type, cacheName, Xscope.LOCAL, xcodeml);

    if(stmt == null) {

      XassignStatement cache1 =
          XelementHelper.createEmpty(XassignStatement.class, xcodeml);
      cache1.appendToChildren(cacheVar, false);
      cache1.appendToChildren(rhs, true);
      XelementHelper.insertAfter(_claw.getPragma(), cache1);
    } else {
      /*
       * We replace an assignement of type
       * A = B
       * by
       * cache_A = B
       * A = cache_A
       */
      XassignStatement cache1 =
          XelementHelper.createEmpty(XassignStatement.class, xcodeml);
      cache1.appendToChildren(cacheVar, false);
      cache1.appendToChildren(stmt.getExprModel().getElement(), true);
      XassignStatement cache2 =
          XelementHelper.createEmpty(XassignStatement.class, xcodeml);
      cache2.appendToChildren(stmt.getLValueModel().getElement(), true);
      cache2.appendToChildren(cacheVar, true);
      XelementHelper.insertAfter(stmt, cache1);
      XelementHelper.insertAfter(cache1, cache2);

    }
    return cacheVar;
  }

  private List<XarrayRef> checkOffsetAndGetArrayRefs(XcodeProgram xcodeml,
                                                     XfunctionDefinition fctDef,
                                                     String var)
      throws IllegalTransformationException
  {
    List<Integer> offsets = _claw.getOffsets();
    if(offsets.size() == 0){
      offsets = generateInferredOffsets(xcodeml, fctDef, var);
    }

    List<XarrayRef> arrayRefs =
        XelementHelper.getAllArrayReferencesByOffsets(_doStmt.getBody(),
            var, offsets);
    if(arrayRefs.size() == 0){
      throw new IllegalTransformationException("Variable " + var +
          " defined in the data clause has not been found",
          _claw.getPragma().getLineNo()
      );
    }
    return arrayRefs;
  }

  /**
   * Update the array references with the newly created cache variable.
   * @param arrayRefs The list of array references to be updated.
   * @param cache     The new cache variable.
   */
  private void updateArrayRefWithCache(List<XarrayRef> arrayRefs, Xvar cache){
    for(XarrayRef ref : arrayRefs){
      // Swap arrayRef with the cache variable
      XelementHelper.insertAfter(ref, cache.cloneObject());
      ref.delete();
    }
  }
}
