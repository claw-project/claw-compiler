/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

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
    _doStmt = XelementHelper.findParentDoStmt(_stmt);
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
    if(_claw.hasDataClause()){
      transformData(xcodeml, transformer);
    } else {
      transformAssignStmt(xcodeml, transformer);
    }
  }

  /**
   * Apply the tranformation for the data list.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @throws Exception If smth prevent the transformation to be done.
   */
  private void transformData(XcodeProgram xcodeml, Transformer transformer)
      throws Exception
  {
    // 1. Find the function/module declaration
    // TODO find parent definition with symbols and declaration table
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());


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

    // 2. introduce the scalar variable for caching
    String arrayVarName =
        _stmt.getLValueModel().getArrayRef().getVarRef().getVar().getValue();
    String cacheName =
        generateNameWithOffsetInfo(arrayVarName, _claw.getOffsets());

    // TODO check for type if it is correct one in any case
    String type = _stmt.getLValueModel().getArrayRef().getType();
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


    /*
     * 2.4 add new assign statement
     * We replace an assignement of type
     * A = B
     * by
     * cache_A = B
     * A = cache_A
     */
    Xvar cacheVar = Xvar.create(type, cacheName, Xscope.LOCAL, xcodeml);
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

    _stmt.delete();
    _claw.getPragma().delete();

    // 2. Insert init statement if needed
    applyInitClause(xcodeml);

    // 3. Update array refences at given offset.
    List<XarrayRef> arrayRefs =
        XelementHelper.getAllArrayReferencesByOffsets(_doStmt.getBody(),
            arrayVarName, _claw.getOffsets());
    for(XarrayRef ref : arrayRefs){
      // Swap arrayRef with the cache variable
      XelementHelper.insertAfter(ref, cacheVar.cloneObject());
      ref.delete();
    }
  }

  /**
   * Apply the init clause if it was part of the kcache directive.
   * @param xcodeml Current program in which the transformation is performed.
   * @throws IllegalTransformationException
   */
  private void applyInitClause(XcodeProgram xcodeml)
    throws IllegalTransformationException
  {
    if(_claw.hasInitClause()){
      XifStatement initIfStmt = XifStatement.create(xcodeml);
      XbinaryExpr logEq =
          XelementHelper.createEmpty(XelementName.LOG_EQ_EXPR, xcodeml);

      // Set lhs of equality
      logEq.appendToChildren(_doStmt.getIterationRange().getInductionVar(),
          true);
      // Set rhs of equality
      logEq.appendToChildren(_doStmt.getIterationRange().getIndexRange().
          getLowerBound().getExprModel().getElement(), true);

      initIfStmt.getCondition().appendToChildren(logEq, false);



      //TODO

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
}
