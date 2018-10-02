/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.ll.loop;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Range;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.translator.ClawTranslator;

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
public class ArrayTransform extends ClawBlockTransformation {

  private final List<List<Xnode>> _groupIterationRanges;
  private final List<List<Xnode>> _groupedAssignStmts;

  /**
   * Constructs a new ArrayTransform triggered from a specific directive.
   *
   * @param begin The directive that triggered the array transform
   *              transformation.
   * @param end   The directive that close the block transformation.
   *              Can be null.
   */
  public ArrayTransform(ClawPragma begin, ClawPragma end) {
    super(begin, end);
    _groupedAssignStmts = new ArrayList<>();
    _groupIterationRanges = new ArrayList<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    if(_clawEnd != null) { // Block transformation

      // TODO Analyse dependency between assignments. cf array9 example.

      // Find assignments with array notation
      List<Xnode> foundAssignments =
          XnodeUtil.getArrayAssignInBlock(_clawStart.getPragma(),
              _clawEnd.getPragma().value()
          );

      if(foundAssignments.isEmpty()) {
        xcodeml.addError(
            "No array notation assignments found in the array-transform block.",
            _clawStart.getPragma().lineNo()
        );
        return false;
      }

      /* Using a structure of list of list of assignments to group together the
       * array notation that share an identical iteration range. */

      // 1st group always exists
      _groupedAssignStmts.add(new ArrayList<Xnode>());
      int crtGroup = 0;
      Xnode refArrayRef = foundAssignments.get(0).matchSeq(Xcode.F_ARRAY_REF);
      List<Xnode> refRanges =
          XnodeUtil.getIdxRangesFromArrayRef(refArrayRef);

      // First array notation is automatically in the 1st group as 1st element
      _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(0));
      _groupIterationRanges.add(refRanges);

      for(int i = 1; i < foundAssignments.size(); ++i) {
        Xnode arrayRef =
            foundAssignments.get(i).matchSeq(Xcode.F_ARRAY_REF);
        List<Xnode> ranges =
            XnodeUtil.getIdxRangesFromArrayRef(arrayRef);

        // ranges are not identical so
        if(!Range.compare(refRanges, ranges)) {
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
          _clawStart.getPragma().matchSibling(Xcode.F_ASSIGN_STATEMENT);
      if(stmt == null) {
        xcodeml.addError("Directive not followed by an assign statement",
            _clawStart.getPragma().lineNo());
        return false;
      }
      // Check if we are dealing with an array notation
      if(stmt.child(0).opcode() != Xcode.F_ARRAY_REF) {
        xcodeml.addError("Assign statement is not an array notation",
            _clawStart.getPragma().lineNo());
        return false;
      }

      List<Xnode> ranges = new ArrayList<>();
      for(Xnode el : stmt.child(0).children()) {
        if(el.opcode() == Xcode.INDEX_RANGE) {
          ranges.add(el);
        }
      }
      if(ranges.isEmpty()) {
        xcodeml.addError("Assign statement is not an array notation",
            _clawStart.getPragma().lineNo());
        return false;
      }

      _groupedAssignStmts.add(Collections.singletonList(stmt));
      _groupIterationRanges.add(ranges);
      return true;
    }
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // independent transformation
  }

  /**
   * Transform an assignment using array notation to a do statement.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @param other      Only for dependent transformation. The other
   *                   transformation part of the transformation.
   * @throws Exception If the transformation cannot be applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other) throws Exception
  {
    ClawTranslator ct = (ClawTranslator) translator;

    // 1. Find the function/module declaration TODO handle module/program ?
    FfunctionDefinition fctDef = _clawStart.getPragma().findParentFunction();
    Xnode grip = _clawStart.getPragma();
    for(int i = 0; i < _groupedAssignStmts.size(); ++i) {
      grip = generateDoStmtNotation(xcodeml, ct, fctDef,
          _groupIterationRanges.get(i), _groupedAssignStmts.get(i), grip);
    }
    removePragma();
    transformed();
  }

  /**
   * Generate the corresponding do statements for the array notations. A do
   * statement is generated per dimension of the arrays. Iteration index range
   * are computed with array dimensions.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @param fctDef     The function definition in which the array notation is
   *                   nested.
   * @param ranges     The list of iteration ranges to be applied to the
   *                   created do statements.
   * @param statements The list of assign statements (array notation) that will
   *                   be included in the nested do statements.
   * @param doStmtGrip Grip for the code insertion. Do statements will be
   *                   inserted after the grip element.
   * @return The last stmt created to be used as a grip for next insertion.
   */
  private Xnode generateDoStmtNotation(XcodeProgram xcodeml,
                                       ClawTranslator translator,
                                       FfunctionDefinition fctDef,
                                       List<Xnode> ranges,
                                       List<Xnode> statements,
                                       Xnode doStmtGrip)
      throws IllegalTransformationException
  {
    String[] inductionVars = new String[ranges.size()];
    Xnode[] doStmts = new Xnode[ranges.size()];
    Xnode var =
        statements.get(0).matchSeq(Xcode.F_ARRAY_REF, Xcode.VAR_REF, Xcode.VAR);
    // 1. Create do statements with induction variables
    for(int i = 0; i < ranges.size(); ++i) {
      // 1.1 Create induction variables
      if(_clawStart.hasInductionClause()) { // Use user names
        inductionVars[i] = _clawStart.getInductionValues().get(i);
      } else { // generate new names
        inductionVars[i] = "claw_induction_" +
            translator.getNextTransformationCounter();
      }

      // 2.2 inject a new entry in the symbol table
      if(!fctDef.getSymbolTable().contains(inductionVars[i])) {
        Xid inductionVarId = xcodeml.createId(FortranType.INTEGER,
            XstorageClass.F_LOCAL, inductionVars[i]);
        fctDef.getSymbolTable().add(inductionVarId, false);
      }

      // 2.3 inject a new entry in the declaration table
      if(!fctDef.getDeclarationTable().contains(inductionVars[i])) {
        Xnode inductionVarDecl =
            xcodeml.createVarDecl(FortranType.INTEGER, inductionVars[i]);
        fctDef.getDeclarationTable().add(inductionVarDecl);
      }

      // 2.4 create do statements
      Xnode inductionVar = xcodeml.
          createVar(FortranType.INTEGER, inductionVars[i], Xscope.LOCAL);
      Xnode range;
      if(ranges.get(i).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)) {
        // Allocatable array
        // dimension argument of size starts at one
        range = xcodeml.createRangeForAssumedShapeArray(var, 1, i + 1);
      } else {
        range = ranges.get(i).cloneNode();
      }
      doStmts[i] = xcodeml.createDoStmt(inductionVar, range);
      statements.get(0).copyEnhancedInfo(doStmts[i]);
      if(i == 0) { // most outer loop goes after the pragma
        doStmtGrip.insertAfter(doStmts[i]);
      } else { // others loop go in the previous one
        doStmts[i - 1].body().append(doStmts[i]);
      }
    }

    for(Xnode stmt : statements) {
      // 3. Adapt array reference with induction variables
      List<Xnode> allArrayRef = stmt.matchAll(Xcode.F_ARRAY_REF);
      for(Xnode arrayRef : allArrayRef) {
        for(int i = 0; i < arrayRef.children().size() - 1; ++i) {
          Xnode el = arrayRef.child(i + 1);
          if(el.opcode() == Xcode.INDEX_RANGE) {
            String induction = doStmts[i].matchSeq(Xcode.VAR).value();
            Xnode inductionVar =
                xcodeml.createVar(FortranType.INTEGER, induction, Xscope.LOCAL);

            Xnode arrayIdx = xcodeml.createNode(Xcode.ARRAY_INDEX);
            arrayIdx.append(inductionVar);

            el.insertAfter(arrayIdx);
            el.delete();
          }
        }
      }

      // 4. Move assignment statement inside the most inner loop
      doStmts[ranges.size() - 1].body().append(stmt, true);
      stmt.delete();
    }

    // Generate directive pragmas if needed

    Xnode grip = null;
    if(_clawStart.hasAcceleratorClause()) {
      /* TODO
         OpenACC and OpenMP loop construct are pretty different ...
         have to look how to do that properly. See issue #22
       */
      grip = Directive.generateAcceleratorClause(xcodeml, doStmts[0],
          _clawStart.getAcceleratorClauses());
    }

    if(_clawStart.hasParallelClause()) {
      grip = Directive.generateParallelClause(xcodeml,
          (grip == null) ? doStmts[0] : grip, doStmts[0]);
    }

    // Add any additional transformation defined in the directive clauses
    translator.generateAdditionalTransformation(_clawStart, xcodeml,
        doStmts[0]);
    return grip == null ? doStmts[0] : grip;
  }
}
