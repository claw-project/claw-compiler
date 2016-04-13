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
import java.util.Arrays;
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
  private XassignStatement _stmt;
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
    if(_claw.hasDataClause()){
      return true; // Analysis is not done at this time
    }

    // Only for assign statement cache from here

    // pragma must be followed by an assign statement
    _stmt = XelementHelper.findDirectNextAssignStmt(_claw.getPragma());
    if(_stmt == null){
      xcodeml.addError("Directive not follwed by an assign statement",
          _claw.getPragma().getLineNo());
      return false;
    }
    // Check if the LHS of the assign stmt is an array reference
    if(!_stmt.getLValueModel().isArrayRef()){
      xcodeml.addError("LHS of assign statement is not an array reference",
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
    if(_claw.hasDataClause()){
      transformData(xcodeml);
    } else {
      transformAssignStmt(xcodeml, transformer);
    }
  }

  /**
   * Apply the tranformation for the data list.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @throws Exception If smth prevent the transformation to be done.
   */
  private void transformData(XcodeProgram xcodeml)
      throws Exception
  {
    // 1. Find the function/module declaration
    // TODO find parent definition with symbols and declaration table
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());

    List<String> privateVars = new ArrayList<>();
    for(String var : _claw.getDataClauseValues()){
      List<XarrayRef> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, var);

      // Generate the cache variable and its assignment
      String type = aRefs.get(0).getType();
      Xvar cacheVar = generateCacheVarAndAssignStmt(xcodeml, var, type, fctDef,
          aRefs.get(0));

      updateArrayRefWithCache(aRefs, cacheVar);
      privateVars.add(cacheVar.getValue());
    }

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, _claw.getPragma(),
        privateVars);
    _claw.getPragma().delete();
  }

  /**
   * Apply the transformation for the LHS array reference.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @throws Exception If smth prevent the transformation to be done.
   */
  private void transformAssignStmt(XcodeProgram xcodeml,
                                   Transformer transformer) throws Exception
  {
    // 1. Find the function/module declaration
    // TODO find parent definition with symbols and declaration table
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());

    String var =
        _stmt.getLValueModel().getArrayRef().getVarRef().getVar().getValue();

    String type = _stmt.getLValueModel().getArrayRef().getType();
    Xvar cacheVar =
        generateCacheVarAndAssignStmt(xcodeml, var, type, fctDef, _stmt);

    List<XarrayRef> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, var);

    applyInitClause(xcodeml, transformer, cacheVar, aRefs.get(0));

    updateArrayRefWithCache(aRefs, cacheVar);

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, _claw.getPragma(),
        Collections.singletonList(cacheVar.getValue()));
    _stmt.delete();
    _claw.getPragma().delete();
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
   * @return The new created Xvar element.
   * @throws IllegalTransformationException
   */
  private Xvar generateCacheVarAndAssignStmt(XcodeProgram xcodeml, String var,
                                             String type,
                                             XfunctionDefinition fctDef,
                                             XbaseElement rhs)
      throws IllegalTransformationException
  {
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

    if(_claw.hasDataClause()) {

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
      cache1.appendToChildren(_stmt.getExprModel().getElement(), true);
      XassignStatement cache2 =
          XelementHelper.createEmpty(XassignStatement.class, xcodeml);
      cache2.appendToChildren(_stmt.getLValueModel().getElement(), true);
      cache2.appendToChildren(cacheVar, true);
      XelementHelper.insertAfter(_stmt, cache1);
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
