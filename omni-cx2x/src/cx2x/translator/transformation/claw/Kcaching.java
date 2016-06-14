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
import cx2x.xcodeml.xnode.Xattr;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

import java.util.ArrayList;
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
  private Xnode _doStmt;
  private Xnode _pragma;

  /**
   * Constructs a new Kcaching transformation triggered from a specific pragma.
   * @param directive The directive that triggered the k caching transformation.
   */
  public Kcaching(ClawLanguage directive) {
    super(directive);
    _claw = directive;
    // TODO XNODE remove after refactoring
    _pragma = new Xnode(_claw.getPragma().getBaseElement());
  }

  /**
   * @see Transformation#analyze(XcodeProgram, Transformer)
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // TODO XNODE pargma should be Xnode directly after refactory
    _doStmt = XelementHelper.findParent(Xcode.FDOSTATEMENT,
        new Xnode(_claw.getPragma().getBaseElement()));
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
    // independent transformation
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
    // TODO XNODE pargma should be Xnode directly after refactory
    _doStmt = XelementHelper.findParent(Xcode.FDOSTATEMENT,
        new Xnode(_claw.getPragma().getBaseElement()));

    // Check if there is an assignment

    // 1. Find the function/module declaration
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());

    for(String data : _claw.getDataClauseValues()){
      Xnode stmt = XelementHelper.getFirstArrayAssign(_pragma, data);

      boolean standardArrayRef = true;
      if(stmt != null) {
        for (Xnode el : stmt.findNode(Xcode.FARRAYREF).getChildren()) {
          if (el.Opcode() == Xcode.ARRAYINDEX) {

            if (!(el.findNode(Xcode.VAR) != null ||
                el.findNode(Xcode.FINTCONSTANT) != null)) {
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

    List<Xnode> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, data);

    // Generate the cache variable and its assignment
    String type = aRefs.get(0).getAttribute(Xattr.TYPE);
    Xnode cacheVar = generateCacheVarAndAssignStmt(xcodeml, data, type, fctDef,
        aRefs.get(0), null);

    updateArrayRefWithCache(aRefs, cacheVar);

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, transformer,
        _pragma, cacheVar.getValue());
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
                                   Xnode stmt,
                                   Transformer transformer) throws Exception
  {
    String type = stmt.findNode(Xcode.FARRAYREF).getAttribute(Xattr.TYPE);
    List<Xnode> aRefs = checkOffsetAndGetArrayRefs(xcodeml, fctDef, data);

    Xnode cacheVar =
        generateCacheVarAndAssignStmt(xcodeml, data, type, fctDef, stmt, stmt);

    applyInitClause(xcodeml, transformer, cacheVar, aRefs.get(0));

    updateArrayRefWithCache(aRefs, cacheVar);

    AcceleratorHelper.generatePrivateClause(_claw, xcodeml, transformer,
        _pragma, cacheVar.getValue());
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
                               Xnode cacheVar, Xnode arrayRef)
    throws IllegalTransformationException
  {

    if(_claw.hasInitClause()){
      ClawTransformer ct = (ClawTransformer)transformer;
      Xnode initIfStmt = (Xnode) ct.hasElement(_doStmt);
      if(initIfStmt == null){
        // If statement has not been created yet so we do it here
        initIfStmt = XelementHelper.createIfThen(xcodeml);
        XelementHelper.copyEnhancedInfo(_pragma, initIfStmt);
        Xnode logEq = new Xnode(Xcode.LOGEQEXPR, xcodeml);

        // Set lhs of equality
        logEq.appendToChildren(_doStmt.findNode(Xcode.VAR), true);
        // Set rhs of equality
        logEq.appendToChildren(_doStmt.findNode(Xcode.INDEXRANGE).
            findNode(Xcode.LOWERBOUND).getChild(0), true);

        initIfStmt.findNode(Xcode.CONDITION).appendToChildren(logEq, false);
        _doStmt.getBody().insert(initIfStmt, false);
        ct.storeElement(_doStmt, initIfStmt);
      }

      Xnode initAssignment = new Xnode(Xcode.FASSIGNSTATEMENT, xcodeml);
      initAssignment.appendToChildren(cacheVar, true); // set rhs
      initAssignment.appendToChildren(arrayRef, true); // set lhs
      // Add assignment in the "then" body element
      initIfStmt.findNode(Xcode.THEN).getBody().
          appendToChildren(initAssignment, false);
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
  private Xnode generateCacheVarAndAssignStmt(XcodeProgram xcodeml, String var,
                                             String type,
                                             XfunctionDefinition fctDef,
                                             Xnode rhs,
                                             Xnode stmt)
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
    Xnode cacheVar =
        XelementHelper.createVar(type, cacheName, Xscope.LOCAL, xcodeml);

    if(stmt == null) {
      Xnode cache1 = new Xnode(Xcode.FASSIGNSTATEMENT, xcodeml);
      cache1.appendToChildren(cacheVar, false);
      cache1.appendToChildren(rhs, true);
      XelementHelper.insertAfter(_pragma, cache1);
    } else {
      /*
       * We replace an assignement of type
       * A = B
       * by
       * cache_A = B
       * A = cache_A
       */
      Xnode cache1 = new Xnode(Xcode.FASSIGNSTATEMENT, xcodeml);
      cache1.appendToChildren(cacheVar, false);
      cache1.appendToChildren(stmt.getChild(1), true);
      Xnode cache2 = new Xnode(Xcode.FASSIGNSTATEMENT, xcodeml);
      cache2.appendToChildren(stmt.getChild(0), true);
      cache2.appendToChildren(cacheVar, true);
      XelementHelper.insertAfter(stmt, cache1);
      XelementHelper.insertAfter(cache1, cache2);

    }
    return cacheVar;
  }

  private List<Xnode> checkOffsetAndGetArrayRefs(XcodeProgram xcodeml,
                                                     XfunctionDefinition fctDef,
                                                     String var)
      throws IllegalTransformationException
  {
    List<Integer> offsets = _claw.getOffsets();
    if(offsets.size() == 0){
      offsets = generateInferredOffsets(xcodeml, fctDef, var);
    }

    List<Xnode> arrayRefs =
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
  private void updateArrayRefWithCache(List<Xnode> arrayRefs, Xnode cache){
    for(Xnode ref : arrayRefs){
      // Swap arrayRef with the cache variable
      XelementHelper.insertAfter(ref, cache.cloneObject());
      ref.delete();
    }
  }
}
