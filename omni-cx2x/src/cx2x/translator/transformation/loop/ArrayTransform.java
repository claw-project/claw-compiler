/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.*;

import java.util.ArrayList;
import java.util.List;

/**
 * <pre>
 * An ArrayTranform transformation is an independent transformation. It
 * transforms the Fortran array notation into single or nested do statements.
 *
 * Array notation example:
 * A(1:n) = A(1+m:n+m) + B(1:n) * C(n+1:n+n)
 *
 * DO i=1,n
 *   A(i) = A(i+m) + B(i) * C(n+i)
 * END DO
 * </pre>
 *
 * @author clementval
 */
public class ArrayTransform extends Transformation {

  private ClawLanguage _claw;
  private XassignStatement _stmt;
  private List<XindexRange> _ranges;


  /**
   * Constructs a new ArrayTransform triggered from a specific directive.
   * @param directive The directive that triggered the array transform
   *                  transformation.
   */
  public ArrayTransform(ClawLanguage directive) {
    super(directive);
    _claw = directive;
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // pragma must be followed by an assign statement
    _stmt = XelementHelper.findDirectNextAssignStmt(_claw.getPragma());
    if(_stmt == null){
      xcodeml.addError("Directive not follwed by an assign statement",
          _claw.getPragma().getLineNo());
      return false;
    }
    // Check if we are dealing with an array notation
    if(!_stmt.getLValueModel().isArrayRef()){
      xcodeml.addError("Assign stament is not an array notation",
          _claw.getPragma().getLineNo());
      return false;
    }

    _ranges = new ArrayList<>();
    for(XbaseElement el :
        _stmt.getLValueModel().getArrayRef().getInnerElements())
    {
      if(el instanceof XindexRange){
        _ranges.add((XindexRange) el);
      }
    }
    if(_ranges.size() == 0){
      xcodeml.addError("Assign stament is not an array notation",
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
   * Transform an assignement using array notation to a do statement.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @param other       Only for dependent transformation. The other
   *                    transformation part of the transformation.
   * @throws Exception If the thransformation cannot be applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
    // 1. Find the function/module declaration
    // TODO hanlde module/program as well
    XfunctionDefinition fctDef =
        XelementHelper.findParentFctDef(_claw.getPragma());

    String[] inductionVars = new String[_ranges.size()];
    XdoStatement[] doStmts = new XdoStatement[_ranges.size()];

    // 2. Create do statements with induction variables
    for(int i = 0; i < _ranges.size(); ++i){
      // 2.1 Create induction variables
      inductionVars[i] = "claw_induction_" +
          transformer.getNextTransformationCounter();

      // 2.2 inject a new entry in the symbol table
      Xid inductionVarId = Xid.create(XelementName.TYPE_F_INT,
          XelementName.SCLASS_F_LOCAL, inductionVars[i], xcodeml);
      fctDef.getSymbolTable().add(inductionVarId, false);

      // 2.3 inject a new entry in the declaration table
      XvarDecl inductionVarDecl = XvarDecl.create(XelementName.TYPE_F_INT,
          inductionVars[i], xcodeml);
      fctDef.getDeclarationTable().add(inductionVarDecl);

      // 2.4 create do statements
      Xvar inductionVar = Xvar.create(XelementName.TYPE_F_INT, inductionVars[i],
          Xscope.LOCAL, xcodeml);
      XindexRange range = _ranges.get(i).cloneObject();
      doStmts[i] = XdoStatement.create(inductionVar, range, false, xcodeml);

      if(i == 0){ // most outter loop goes after the pragma
        XelementHelper.insertAfter(_claw.getPragma(), doStmts[i]);
      } else { // others loop go in the previous one
        doStmts[i-1].getBody().appendToChildren(doStmts[i], false);
      }
    }

    // 3. Adapat array reference with induction variables
    List<XarrayRef> allArrayRef = XelementHelper.getAllArrayReferences(_stmt);
    for(XarrayRef arrayRef : allArrayRef) {

      // TODO handle more complicated cases
      String id = arrayRef.getVarRef().getVar().getValue();

      for(int i = 0; i < arrayRef.getInnerElements().size(); ++i){
        XbaseElement el = arrayRef.getInnerElements().get(i);
        if(el instanceof XindexRange){
          String induction = doStmts[i].getInductionVariable();

          Xvar iterVar = Xvar.create(XelementName.TYPE_F_INT, induction,
              Xscope.LOCAL, xcodeml);
          XarrayIndex arrayIdx =
              XarrayIndex.create(new XexprModel(iterVar), xcodeml);

          XelementHelper.insertAfter(el, arrayIdx);
          el.delete();
        }
      }
    }
    


    // 4. Move assignment statement inside the most inner loop
    doStmts[_ranges.size()-1].getBody().appendToChildren(_stmt, true);
    _stmt.delete();

    this.transformed();
  }
}
