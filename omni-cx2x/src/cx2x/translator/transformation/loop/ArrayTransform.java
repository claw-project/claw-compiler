/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.helper.TransformationHelper;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.transformation.ClawBlockTransformation;
import cx2x.xcodeml.helper.XnodeUtil;
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
  public ArrayTransform(ClawLanguage begin, ClawLanguage end) {
    super(begin, end);
    _groupedAssignStmts = new ArrayList<>();
    _groupIterationRanges = new ArrayList<>();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    if(_clawEnd != null) { // Block transformation

      // TODO Analyse dependency between assignments. cf array9 example.

      // Find assignments with array notation
      List<Xnode> foundAssignments =
          XnodeUtil.getArrayAssignInBlock(_clawStart.getPragma(),
              _clawEnd.getPragma().value()
          );

      if(foundAssignments.size() == 0) {
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
      Xnode refArrayRef = foundAssignments.get(0).matchSeq(Xcode.FARRAYREF);
      List<Xnode> refRanges =
          XnodeUtil.getIdxRangesFromArrayRef(refArrayRef);

      // First array notation is automatically in the 1st group as 1st element
      _groupedAssignStmts.get(crtGroup).add(foundAssignments.get(0));
      _groupIterationRanges.add(refRanges);

      for(int i = 1; i < foundAssignments.size(); ++i) {
        Xnode arrayRef =
            foundAssignments.get(i).matchSeq(Xcode.FARRAYREF);
        List<Xnode> ranges =
            XnodeUtil.getIdxRangesFromArrayRef(arrayRef);

        // ranges are not identical so
        if(!XnodeUtil.compareIndexRanges(refRanges, ranges)) {
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
      Xnode stmt = _clawStart.getPragma().matchSibling(Xcode.FASSIGNSTATEMENT);
      if(stmt == null) {
        xcodeml.addError("Directive not followed by an assign statement",
            _clawStart.getPragma().lineNo());
        return false;
      }
      // Check if we are dealing with an array notation
      if(!(stmt.child(0).opcode() == Xcode.FARRAYREF)) {
        xcodeml.addError("Assign statement is not an array notation",
            _clawStart.getPragma().lineNo());
        return false;
      }

      List<Xnode> ranges = new ArrayList<>();
      for(Xnode el : stmt.child(0).children()) {
        if(el.opcode() == Xcode.INDEXRANGE) {
          ranges.add(el);
        }
      }
      if(ranges.size() == 0) {
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
    // independent transformation
    return false;
  }

  /**
   * Transform an assignment using array notation to a do statement.
   *
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
        XnodeUtil.findParentFunction(_clawStart.getPragma());
    Xnode grip = _clawStart.getPragma();
    for(int i = 0; i < _groupedAssignStmts.size(); ++i) {
      grip = generateDoStmtNotation(xcodeml, transformer, fctDef,
          _groupIterationRanges.get(i), _groupedAssignStmts.get(i), grip);
    }
    _clawStart.getPragma().delete();
    if(_clawEnd != null) {
      _clawEnd.getPragma().delete();
    }
    this.transformed();
  }

  /**
   * Generate the corresponding do statements for the array notations. A do
   * statement is generated per dimension of the arrays. Iteration index range
   * are computed with array dimensions.
   *
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
   */
  private Xnode generateDoStmtNotation(XcodeProgram xcodeml,
                                       Transformer transformer,
                                       XfunctionDefinition fctDef,
                                       List<Xnode> ranges,
                                       List<Xnode> statements,
                                       Xnode doStmtGrip)
  {
    String[] inductionVars = new String[ranges.size()];
    Xnode[] doStmts = new Xnode[ranges.size()];
    Xnode var =
        statements.get(0).matchSeq(Xcode.FARRAYREF, Xcode.VARREF, Xcode.VAR);
    // 1. Create do statements with induction variables
    for(int i = 0; i < ranges.size(); ++i) {
      // 1.1 Create induction variables
      if(_clawStart.hasInductionClause()) { // Use user names
        inductionVars[i] = _clawStart.getInductionValues().get(i);
      } else { // generate new names
        inductionVars[i] = "claw_induction_" +
            transformer.getNextTransformationCounter();
      }

      // 2.2 inject a new entry in the symbol table
      if(!fctDef.getSymbolTable().contains(inductionVars[i])) {
        Xid inductionVarId = xcodeml.createId(Xname.TYPE_F_INT,
            Xname.SCLASS_F_LOCAL, inductionVars[i]);
        fctDef.getSymbolTable().add(inductionVarId, false);
      }

      // 2.3 inject a new entry in the declaration table
      if(!fctDef.getDeclarationTable().contains(inductionVars[i])) {
        Xdecl inductionVarDecl =
            xcodeml.createVarDecl(Xname.TYPE_F_INT, inductionVars[i]);
        fctDef.getDeclarationTable().add(inductionVarDecl);
      }

      // 2.4 create do statements
      Xnode inductionVar = xcodeml.createVar(Xname.TYPE_F_INT, inductionVars[i],
          Xscope.LOCAL);
      Xnode range;
      if(ranges.get(i).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)) {
        // Allocatable array
        // dimension argument of size starts at one
        range = xcodeml.createRangeForAssumedShapeArray(var, 1, i + 1);
      } else {
        range = ranges.get(i).cloneNode();
      }
      doStmts[i] = xcodeml.createDoStmt(inductionVar, range);
      XnodeUtil.copyEnhancedInfo(statements.get(0), doStmts[i]);
      if(i == 0) { // most outer loop goes after the pragma
        doStmtGrip.insertAfter(doStmts[i]);
      } else { // others loop go in the previous one
        doStmts[i - 1].body().append(doStmts[i], false);
      }
    }


    for(Xnode stmt : statements) {
      // 3. Adapt array reference with induction variables
      List<Xnode> allArrayRef = stmt.matchAll(Xcode.FARRAYREF);
      for(Xnode arrayRef : allArrayRef) {
        for(int i = 0; i < arrayRef.children().size() - 1; ++i) {
          Xnode el = arrayRef.child(i + 1);
          if(el.opcode() == Xcode.INDEXRANGE) {
            String induction = doStmts[i].matchSeq(Xcode.VAR).value();
            Xnode inductionVar =
                xcodeml.createVar(Xname.TYPE_F_INT, induction, Xscope.LOCAL);

            Xnode arrayIdx = new Xnode(Xcode.ARRAYINDEX, xcodeml);
            arrayIdx.append(inductionVar, false);

            el.insertAfter(arrayIdx);
            el.delete();
          }
        }
      }

      // 4. Move assignment statement inside the most inner loop
      doStmts[ranges.size() - 1].body().append(stmt, true);
      stmt.delete();
    }


    // Generate accelerator pragmas if needed
    Xnode potentialGrip = AcceleratorHelper.generateAdditionalDirectives(
        _clawStart, xcodeml, new Xnode(doStmts[0].element()),
        new Xnode(doStmts[0].element()));

    // Add any additional transformation defined in the directive clauses
    TransformationHelper.generateAdditionalTransformation(_clawStart, xcodeml,
        transformer, doStmts[0]);

    return potentialGrip == null ? doStmts[0] : potentialGrip;
  }
}
