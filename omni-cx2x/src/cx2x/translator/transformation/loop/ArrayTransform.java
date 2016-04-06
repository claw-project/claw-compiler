/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.common.Constant;
import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.BlockTransformation;
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
public class ArrayTransform extends BlockTransformation {

  private final ClawLanguage _clawBegin, _clawEnd;
  private XassignStatement _stmt;
  private List<XindexRange> _ranges;


  /**
   * Constructs a new ArrayTransform triggered from a specific directive.
   * @param begin The directive that triggered the array transform
   *              transformation.
   * @param end   The directive that close the block transformation.
   *              Can be null.
   */
  public ArrayTransform(AnalyzedPragma begin, AnalyzedPragma end){
    super(begin, end);
    _clawBegin = (ClawLanguage) begin;
    _clawEnd = (ClawLanguage) end;
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    if(_clawEnd != null){ // Block transformation



      return true;
    } else { // single transformation
      // pragma must be followed by an assign statement
      _stmt = XelementHelper.findDirectNextAssignStmt(_clawBegin.getPragma());
      if(_stmt == null){
        xcodeml.addError("Directive not follwed by an assign statement",
            _clawBegin.getPragma().getLineNo());
        return false;
      }
      // Check if we are dealing with an array notation
      if(!_stmt.getLValueModel().isArrayRef()){
        xcodeml.addError("Assign stament is not an array notation",
            _clawBegin.getPragma().getLineNo());
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
            _clawBegin.getPragma().getLineNo());
        return false;
      }
      return true;
    }
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
    if(_clawEnd != null) { // Block transformation

    } else {  // Single transformation


      // 1. Find the function/module declaration
      // TODO hanlde module/program as well
      XfunctionDefinition fctDef =
          XelementHelper.findParentFctDef(_clawBegin.getPragma());

      String[] inductionVars = new String[_ranges.size()];
      XdoStatement[] doStmts = new XdoStatement[_ranges.size()];

      // 2. Create do statements with induction variables
      for (int i = 0; i < _ranges.size(); ++i) {
        // 2.1 Create induction variables
        if(_clawBegin.hasInductionOption()){ // Use user names
          inductionVars[i] = _clawBegin.getInductionNames().get(i);
        } else { // genarate new names
          inductionVars[i] = "claw_induction_" +
              transformer.getNextTransformationCounter();
        }

        // 2.2 inject a new entry in the symbol table
        if(!fctDef.getSymbolTable().contains(inductionVars[i])){
          Xid inductionVarId = Xid.create(XelementName.TYPE_F_INT,
              XelementName.SCLASS_F_LOCAL, inductionVars[i], xcodeml);
          fctDef.getSymbolTable().add(inductionVarId, false);
        }

        // 2.3 inject a new entry in the declaration table
        if(!fctDef.getDeclarationTable().contains(inductionVars[i])){
          XvarDecl inductionVarDecl = XvarDecl.create(XelementName.TYPE_F_INT,
              inductionVars[i], xcodeml);
          fctDef.getDeclarationTable().add(inductionVarDecl);
        }

        // 2.4 create do statements
        Xvar inductionVar = Xvar.create(XelementName.TYPE_F_INT, inductionVars[i],
            Xscope.LOCAL, xcodeml);
        XindexRange range = _ranges.get(i).cloneObject();
        doStmts[i] = XdoStatement.create(inductionVar, range, false, xcodeml);

        if (i == 0) { // most outter loop goes after the pragma
          XelementHelper.insertAfter(_clawBegin.getPragma(), doStmts[i]);
        } else { // others loop go in the previous one
          doStmts[i - 1].getBody().appendToChildren(doStmts[i], false);
        }
      }

      // 3. Adapat array reference with induction variables
      List<XarrayRef> allArrayRef = XelementHelper.getAllArrayReferences(_stmt);
      for (XarrayRef arrayRef : allArrayRef) {

        // TODO handle more complicated cases
        String id = arrayRef.getVarRef().getVar().getValue();

        for (int i = 0; i < arrayRef.getInnerElements().size(); ++i) {
          XbaseElement el = arrayRef.getInnerElements().get(i);
          if (el instanceof XindexRange) {
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
      doStmts[_ranges.size() - 1].getBody().appendToChildren(_stmt, true);
      _stmt.delete();



      // TODO move 5.1 & 5.2 in a centralized place as they are common to multiple transformation
      // 5.1 Generate accelerator directive if option is given
      if(_clawBegin.hasAccOption()){
        Xpragma acceleratorPragma = XelementHelper.createEmpty(Xpragma.class,
            xcodeml);
        acceleratorPragma.setValue(Constant.OPENACC_PREFIX + " " +
            _clawBegin.getAccClauses());
        XelementHelper.insertAfter(_clawBegin.getPragma(), acceleratorPragma);
      }

      // 5.2 Generate accelerator directive for parallel option
      if(_clawBegin.hasParallelOption()){
        Xpragma beginParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        beginParallel.setValue(Constant.OPENACC_PARALLEL);
        Xpragma endParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        endParallel.setValue(Constant.OPENACC_END_PARALLEL);
        XelementHelper.insertAfter(_clawBegin.getPragma(), beginParallel);
        XelementHelper.insertAfter(doStmts[0], endParallel);
      }

      this.transformed();
    }
  }
}
