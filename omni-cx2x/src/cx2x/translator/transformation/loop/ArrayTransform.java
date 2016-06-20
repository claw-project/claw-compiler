/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.BlockTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * <pre>
 * An ArrayTransform transformation is an independent transformation. It
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
  private final List<List<Xnode>> _groupIterationRanges;
  private final List<List<Xnode>> _groupedAssignStmts;

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
    _groupedAssignStmts = new ArrayList<>();
    _groupIterationRanges = new ArrayList<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    if(_clawEnd != null){ // Block transformation

      // TODO Analyse dependency between assignments. cf array9 example.

      // Find assignments with array notation
      List<Xnode> foundAssignments =
          XnodeUtil.getArrayAssignInBlock(_clawBegin.getPragma(),
              _clawEnd.getPragma().getValue()
          );

      if(foundAssignments.size() == 0){
        xcodeml.addError(
            "No array notation assignments found in the array-transform block.",
            _clawBegin.getPragma().getLineNo()
        );
        return false;
      }

      /* Using a structure of list of list of assignments to group together the
       * array notation that share an identical iteration range. */

      // 1st group always exists
      _groupedAssignStmts.add(new ArrayList<Xnode>());
      int crtGroup = 0;
      Xnode refArrayRef = foundAssignments.get(0).find(Xcode.FARRAYREF);
      List<Xnode> refRanges =
          XnodeUtil.getIdxRangesFromArrayRef(refArrayRef);

      // First array notation is automatically in the 1st group as 1st element
      _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(0));
      _groupIterationRanges.add(refRanges);

      for(int i = 1; i < foundAssignments.size(); ++i){
        Xnode arrayRef =
            foundAssignments.get(i).find(Xcode.FARRAYREF);
        List<Xnode> ranges =
            XnodeUtil.getIdxRangesFromArrayRef(arrayRef);

        // ranges are not identical so
        if(!XnodeUtil.compareIndexRanges(refRanges, ranges)){
          refRanges = ranges;
          ++crtGroup;
          _groupedAssignStmts.add(new ArrayList<Xnode>());
          _groupIterationRanges.add(refRanges);
        }
        _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(i));
      }
      return true;
    } else { // single transformation
      // pragma must be followed by an assign statement
      Xnode stmt =
          XnodeUtil.findDirectNext(Xcode.FASSIGNSTATEMENT,
              _clawBegin.getPragma());
      if(stmt == null){
        xcodeml.addError("Directive not followed by an assign statement",
            _clawBegin.getPragma().getLineNo());
        return false;
      }
      // Check if we are dealing with an array notation
      if(!(stmt.getChild(0).Opcode() == Xcode.FARRAYREF)){
        xcodeml.addError("Assign statement is not an array notation",
            _clawBegin.getPragma().getLineNo());
        return false;
      }

      List<Xnode> ranges = new ArrayList<>();
      for(Xnode el : stmt.getChild(0).getChildren()){
        if(el.Opcode() == Xcode.INDEXRANGE){
          ranges.add(el);
        }
      }
      if(ranges.size() == 0){
        xcodeml.addError("Assign statement is not an array notation",
            _clawBegin.getPragma().getLineNo());
        return false;
      }

      _groupedAssignStmts.add(Collections.singletonList(stmt));
      _groupIterationRanges.add(ranges);
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
   * Transform an assignment using array notation to a do statement.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @param other       Only for dependent transformation. The other
   *                    transformation part of the transformation.
   * @throws Exception If the transformation cannot be applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation other) throws Exception
  {
      // 1. Find the function/module declaration TODO handle module/program ?
      XfunctionDefinition fctDef =
          XnodeUtil.findParentFunction(_clawBegin.getPragma());
      Xnode grip = _clawBegin.getPragma();
      for(int i = 0; i < _groupedAssignStmts.size(); ++i){
        grip = generateDoStmtNotation(xcodeml, transformer, fctDef,
            _groupIterationRanges.get(i), _groupedAssignStmts.get(i), grip);
      }
      _clawBegin.getPragma().delete();
      if(_clawEnd != null){
        _clawEnd.getPragma().delete();
      }
      this.transformed();
  }

  /**
   * Generate the corresponding do statements for the array notations. A do
   * statement is generated per dimension of the arrays. Iteration index range
   * are computed with array dimensions.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @param fctDef      The function definition in which the array notation is
   *                    nested.
   * @param ranges      The list of iteration ranges to be applied to the
   *                    created do statements.
   * @param statements  The list of assign statements (array notation) that will
   *                    be included in the nested do statements.
   * @param doStmtGrip  Grip for the code insertion. Do statements will be
   *                    inserted after the grip element.
   * @return The last stmt created to be used as a grip for next insertion.
   * @throws IllegalTransformationException if creation of elements fail.
   */
  private Xnode generateDoStmtNotation(XcodeProgram xcodeml,
                                              Transformer transformer,
                                              XfunctionDefinition fctDef,
                                              List<Xnode> ranges,
                                              List<Xnode> statements,
                                              Xnode doStmtGrip)
      throws IllegalTransformationException
  {
    String[] inductionVars = new String[ranges.size()];
    Xnode[] doStmts = new Xnode[ranges.size()];
    Xnode var =
        statements.get(0).find(Xcode.FARRAYREF, Xcode.VARREF, Xcode.VAR);
    // 1. Create do statements with induction variables
    for (int i = 0; i < ranges.size(); ++i) {
      // 1.1 Create induction variables
      if(_clawBegin.hasInductionClause()){ // Use user names
        inductionVars[i] = _clawBegin.getInductionValues().get(i);
      } else { // generate new names
        inductionVars[i] = "claw_induction_" +
            transformer.getNextTransformationCounter();
      }

      // 2.2 inject a new entry in the symbol table
      if(!fctDef.getSymbolTable().contains(inductionVars[i])){
        Xid inductionVarId = XnodeUtil.createId(xcodeml,
            Xname.TYPE_F_INT, Xname.SCLASS_F_LOCAL,
            inductionVars[i]);
        fctDef.getSymbolTable().add(inductionVarId, false);
      }

      // 2.3 inject a new entry in the declaration table
      if(!fctDef.getDeclarationTable().contains(inductionVars[i])){
        XvarDecl inductionVarDecl =
            XnodeUtil.createVarDecl(xcodeml, Xname.TYPE_F_INT,
                inductionVars[i]);
        fctDef.getDeclarationTable().add(inductionVarDecl);
      }

      // 2.4 create do statements
      Xnode inductionVar =
          XnodeUtil.createVar(Xname.TYPE_F_INT, inductionVars[i],
              Xscope.LOCAL, xcodeml);
      Xnode range;
      if(ranges.get(i).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)){
        // Allocatable array
        // dimension argument of size starts at one
        range = XnodeUtil.createAssumedShapeRange(xcodeml, var, 1, i + 1);
      } else {
        range = ranges.get(i).cloneObject();
      }
      doStmts[i] = XnodeUtil.createDoStmt(xcodeml, inductionVar, range);
      XnodeUtil.copyEnhancedInfo(statements.get(0), doStmts[i]);
      if (i == 0) { // most outer loop goes after the pragma
        XnodeUtil.insertAfter(doStmtGrip, doStmts[i]);
      } else { // others loop go in the previous one
        doStmts[i - 1].getBody().appendToChildren(doStmts[i], false);
      }
    }


    for(Xnode stmt : statements) {
      // 3. Adapt array reference with induction variables
      List<Xnode> allArrayRef =
          XnodeUtil.findAll(Xcode.FARRAYREF, stmt);
      for (Xnode arrayRef : allArrayRef) {
        for (int i = 0; i < arrayRef.getChildren().size() - 1; ++i) {
          Xnode el = arrayRef.getChild(i + 1);
          if (el.Opcode() == Xcode.INDEXRANGE) {
            String induction = doStmts[i].find(Xcode.VAR).getValue();
            Xnode inductionVar =
                XnodeUtil.createVar(Xname.TYPE_F_INT, induction,
                Xscope.LOCAL, xcodeml);

            Xnode arrayIdx = new Xnode(Xcode.ARRAYINDEX, xcodeml);
            arrayIdx.appendToChildren(inductionVar, false);

            XnodeUtil.insertAfter(el, arrayIdx);
            el.delete();
          }
        }
      }

      // 4. Move assignment statement inside the most inner loop
      doStmts[ranges.size() - 1].getBody().appendToChildren(stmt, true);
      stmt.delete();
    }


    // Generate accelerator pragmas if needed
    Xnode potentialGrip = AcceleratorHelper.generateAdditionalDirectives(
        _clawBegin, xcodeml, new Xnode(doStmts[0].getElement()),
        new Xnode(doStmts[0].getElement()));

    // Add any additional transformation defined in the directive clauses
    // TODO XNODE no need to instantiate Xnode after refactoring
    TransformationHelper.generateAdditionalTransformation(_clawBegin, xcodeml,
        transformer, new Xnode(doStmts[0].getElement()));

    return potentialGrip == null ? doStmts[0] : potentialGrip;
  }
}
