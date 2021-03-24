/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import java.util.List;

import claw.tatsu.TatsuConstant;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Message;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.abstraction.HoistedNestedDoStatement;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeML;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Xintrinsic;

/**
 * Primitive transformation and test applied on FdoStatement. This included: -
 * loop fusion (merge) - loop reorder (reorder) - loop hoisting (hoist) -
 * extract body of a loop - check iteration range
 *
 * @author clementval
 */
public final class Loop
{

    private static final String[] prevToDelete = { "acc loop", "omp do" };
    private static final String[] nextToDelete = { "omp end do" };

    // Avoid potential instantiation of this class
    private Loop()
    {
    }

    /**
     * Merge two do statements together. Body of the slave do statement will be
     * append to the body of the master do statement.
     *
     * @param masterDoStmt Master do statement for the merge operation. Stay in
     *                     place. Other do statement will be merged into it.
     * @param slaveDoStmt  Slave do statement for the merge operation. Will be
     *                     merged into the master do statement.
     * @throws IllegalTransformationException If given node are null or not
     *                                        FdoStatement nodes.
     */
    public static void merge(Xnode masterDoStmt, Xnode slaveDoStmt) throws IllegalTransformationException
    {
        if (!Xnode.isOfCode(masterDoStmt, Xcode.F_DO_STATEMENT) || !Xnode.isOfCode(slaveDoStmt, Xcode.F_DO_STATEMENT))
        {
            throw new IllegalTransformationException(String.format("%s for Body.append. opcode: %s - %s",
                    TatsuConstant.ERROR_INCOMPATIBLE, masterDoStmt == null ? "null" : masterDoStmt.opcode(),
                    slaveDoStmt == null ? "null" : slaveDoStmt.opcode()));
        }

        // Merge slave body into the master body
        Body.append(masterDoStmt.body(), slaveDoStmt.body());

        // Delete any acc loop / omp do pragma before/after the slave do statement
        Loop.cleanPragmas(slaveDoStmt, prevToDelete, nextToDelete);
        slaveDoStmt.delete();
    }

    /**
     * Merge two nested do statements group together. Inner most body of the slave
     * group is merged into the inner most body of the master group. Slave group is
     * then deleted.
     *
     * @param master Master nested do statements group for the merge operation.
     * @param slave  Slave nested do statements group for the merge operation.
     * @throws IllegalTransformationException If given node are null or not
     *                                        FdoStatement nodes.
     */
    public static void merge(NestedDoStatement master, NestedDoStatement slave) throws IllegalTransformationException
    {
        if (master == null || master.size() == 0 || slave == null || slave.size() == 0)
        {
            throw new IllegalTransformationException(TatsuConstant.ERROR_INCOMPATIBLE + " for loop fusion");
        }
        merge(master.getInnerStatement(), slave.getInnerStatement());
        slave.getOuterStatement().delete();
    }

    /**
     * Perform a do statements reordering based on the new order specified by
     * induction variables.
     *
     * i,j,k &rarr; k,j,i
     *
     * @param nestedGroup          The nested group of do statements to be
     *                             reordered.
     * @param newInductionVarOrder New order of the induction variables. E.g. k,j,i.
     * @throws IllegalTransformationException If reordering acton is not supported.
     */
    public static void reorder(Context context, NestedDoStatement nestedGroup, List<String> newInductionVarOrder)
            throws IllegalTransformationException
    {
        // Check that new order is possible
        if (nestedGroup.size() == 2)
        { // simple swap
            swapIterationRange(nestedGroup.getOuterStatement(), nestedGroup.getInnerStatement());
            Message.debug(context, "Loop reordering: single swap operation");
        } else if (nestedGroup.size() == 3)
        {
            int newPosition = nestedGroup.computeSwappingIndices(newInductionVarOrder);
            Message.debug(context, "Loop reordering: potential double swap operation " + newPosition);
            switch (newPosition)
            {
            case 201: // Double swap: i,j,k -> j,k,i
                swapIterationRange(nestedGroup.get(0), nestedGroup.get(1));
                swapIterationRange(nestedGroup.get(1), nestedGroup.get(2));
                break;
            case 120: // Double swap: i,j,k -> k,i,j
                swapIterationRange(nestedGroup.get(0), nestedGroup.get(2));
                swapIterationRange(nestedGroup.get(2), nestedGroup.get(1));
                break;
            case 21: // Single swap: i,j,k -> i,k,j
                swapIterationRange(nestedGroup.get(1), nestedGroup.get(2));
                break;
            case 210: // Single swap: i,j,k -> k,j,i
                swapIterationRange(nestedGroup.get(0), nestedGroup.get(2));
                break;
            case 102: // Single swap: i,j,k -> j,i,k
                swapIterationRange(nestedGroup.get(0), nestedGroup.get(1));
                break;
            default:
                // Do nothing.
                break;
            }
        } else
        {
            throw new IllegalTransformationException("Currently unsupported " + "reorder operation.");
        }
    }

    /**
     * Perform a loop hoisting on the given nested do statements.
     *
     * @param hoistedGroups List of groups that will be hoisted.
     * @param start         Starting point of the hoisted loop.
     * @param end           Ending point of the hoisted loop.
     * @param xcodeml       Current XcodeML translation unit for node creation.
     * @return Hoisted nested do statement group.
     * @throws IllegalTransformationException If underlying methods throw exception.
     */
    public static HoistedNestedDoStatement hoist(List<HoistedNestedDoStatement> hoistedGroups, Xnode start, Xnode end,
            XcodeML xcodeml) throws IllegalTransformationException
    {
        // Perform IF extraction and IF creation for lower-bound
        for (HoistedNestedDoStatement g : hoistedGroups)
        {
            if (g.needIfStatement())
            {
                createIfStatementForLowerBound(xcodeml, g);
            }
            Loop.extractBody(g.getInnerStatement(), g.getOuterStatement());
            g.getOuterStatement().delete();
        }

        // Do the hoisting
        HoistedNestedDoStatement hoisted = hoistedGroups.get(0).cloneNestedGroup();
        hoisted.getInnerStatement().body().delete();
        Xnode newBody = xcodeml.createNode(Xcode.BODY);
        hoisted.getInnerStatement().append(newBody);
        Body.shiftIn(start, end, newBody, false);
        start.insertAfter(hoisted.getOuterStatement());
        return hoisted;
    }

    /**
     * Create an IF statement surrounding the entire most inner do statement body.
     * Condition if made from the lower bound (if(induction_var >= lower_bound).
     *
     * @param xcodeml Current XcodeML program
     * @param g       The group of do statements.
     */
    private static void createIfStatementForLowerBound(XcodeML xcodeml, HoistedNestedDoStatement g)
    {
        Xnode ifStmt = xcodeml.createNode(Xcode.F_IF_STATEMENT);
        Xnode condition = xcodeml.createNode(Xcode.CONDITION);
        Xnode thenBlock = xcodeml.createNode(Xcode.THEN);
        g.getOuterStatement().copyEnhancedInfo(ifStmt);
        Xnode cond = xcodeml.createNode(Xcode.LOG_GE_EXPR);
        Xnode inductionVar = g.getOuterStatement().matchDirectDescendant(Xcode.VAR);
        cond.append(inductionVar, true);
        cond.append(g.getOuterStatement().matchDirectDescendant(Xcode.INDEX_RANGE)
                .matchDirectDescendant(Xcode.LOWER_BOUND).child(0), true);
        ifStmt.append(condition);
        ifStmt.append(thenBlock);
        condition.append(cond);
        thenBlock.append(g.getInnerStatement().body(), true);
        g.getInnerStatement().body().delete();
        Xnode body = xcodeml.createNode(Xcode.BODY);
        body.append(ifStmt);
        g.getInnerStatement().append(body);
    }

    /**
     * Swap the iteration range information of two do statement.
     *
     * @param e1 First do statement.
     * @param e2 Second do statement.
     * @throws IllegalTransformationException if necessary elements are missing to
     *                                        apply the transformation.
     */
    private static void swapIterationRange(Xnode e1, Xnode e2) throws IllegalTransformationException
    {
        // The two nodes must be do statement
        if (!Xnode.isOfCode(e1, Xcode.F_DO_STATEMENT) || !Xnode.isOfCode(e2, Xcode.F_DO_STATEMENT))
        {
            throw new IllegalTransformationException("Only two do statement can be " + "swap iteration ranges.");
        }

        Xnode inductionVar1 = e1.matchDirectDescendant(Xcode.VAR);
        Xnode inductionVar2 = e2.matchDirectDescendant(Xcode.VAR);
        Xnode indexRange1 = e1.matchDirectDescendant(Xcode.INDEX_RANGE);
        Xnode indexRange2 = e2.matchDirectDescendant(Xcode.INDEX_RANGE);
        if (inductionVar1 == null || inductionVar2 == null || indexRange1 == null || indexRange2 == null)
        {
            throw new IllegalTransformationException("Induction variable or index " + "range missing.");
        }

        Xnode low1 = indexRange1.matchSeq(Xcode.LOWER_BOUND).child(0);
        Xnode up1 = indexRange1.matchSeq(Xcode.UPPER_BOUND).child(0);
        Xnode s1 = indexRange1.matchSeq(Xcode.STEP).child(0);

        Xnode low2 = indexRange2.matchSeq(Xcode.LOWER_BOUND).child(0);
        Xnode up2 = indexRange2.matchSeq(Xcode.UPPER_BOUND).child(0);
        Xnode s2 = indexRange2.matchSeq(Xcode.STEP).child(0);

        // Set the range of loop2 to loop1
        inductionVar2.insertAfter(inductionVar1.cloneNode());
        low2.insertAfter(low1.cloneNode());
        up2.insertAfter(up1.cloneNode());
        s2.insertAfter(s1.cloneNode());

        inductionVar1.insertAfter(inductionVar2.cloneNode());
        low1.insertAfter(low2.cloneNode());
        up1.insertAfter(up2.cloneNode());
        s1.insertAfter(s2.cloneNode());

        inductionVar1.delete();
        inductionVar2.delete();
        low1.delete();
        up1.delete();
        s1.delete();
        low2.delete();
        up2.delete();
        s2.delete();
    }

    /**
     * Clean up extra pragma that have no more sense after transformation.
     *
     * @param node     Do statement that will be removed.
     * @param previous List of pragma to be removed before the do statement.
     * @param next     List of pragmas to be removed after the do statement.
     */
    private static void cleanPragmas(Xnode node, String[] previous, String[] next)
    {
        if (!Xnode.isOfCode(node, Xcode.F_DO_STATEMENT))
        {
            return;
        }

        Xnode doStatement = node;

        while (Xnode.isOfCode(node.prevSibling(), Xcode.F_PRAGMA_STATEMENT))
        {
            String pragma = node.prevSibling().value().toLowerCase();
            Xnode toDelete = null;

            for (String p : previous)
            {
                if (!pragma.startsWith(CompilerDirective.CLAW.getPrefix()) && pragma.contains(p))
                {
                    toDelete = node.prevSibling();
                    break;
                }
            }

            node = node.prevSibling();
            XnodeUtil.safeDelete(toDelete);
        }

        node = doStatement; // Reset node to the initial position.
        while (Xnode.isOfCode(node.nextSibling(), Xcode.F_PRAGMA_STATEMENT))
        {
            String pragma = node.nextSibling().value().toLowerCase();
            Xnode toDelete = null;

            for (String n : next)
            {
                if (!pragma.startsWith(CompilerDirective.CLAW.getPrefix()) && pragma.contains(n))
                {
                    toDelete = node.nextSibling();
                    break;
                }
            }

            node = node.nextSibling();
            XnodeUtil.safeDelete(toDelete);
        }
    }

    /**
     * Extract the body of a do statement and place it after the reference node.
     *
     * @param loop The do statement containing the body to be extracted.
     * @param ref  Element after which statement are shifted.
     * @throws IllegalTransformationException If node passed as arguments are
     *                                        incompatible with the transformation.
     */
    private static void extractBody(Xnode loop, Xnode ref) throws IllegalTransformationException
    {
        if (ref == null || !Xnode.isOfCode(loop, Xcode.F_DO_STATEMENT))
        {
            throw new IllegalTransformationException(String.format("%s for Loop.extractBody. opcode: %s",
                    TatsuConstant.ERROR_INCOMPATIBLE, loop == null ? "null" : loop.opcode()));
        }
        Xnode body = loop.body();
        if (body == null)
        {
            return;
        }
        Xnode refNode = ref;
        for (Xnode child : body.children())
        {
            refNode.insertAfter(child);
            refNode = child;
        }
    }

    /**
     * Extract the body of a do statement and place it directly after it.
     *
     * @param loop The do statement containing the body to be extracted.
     * @throws IllegalTransformationException If node passed as arguments are
     *                                        incompatible with the transformation.
     */
    public static void extractBody(Xnode loop) throws IllegalTransformationException
    {
        extractBody(loop, loop);
    }

    /**
     * Extract the body of the inner do statement and place it directly after the
     * outer do statement.
     *
     * @param nest Nest do statement group.
     * @throws IllegalTransformationException If node passed as arguments are
     *                                        incompatible with the transformation.
     */
    public static void extractBody(NestedDoStatement nest) throws IllegalTransformationException
    {
        extractBody(nest.getInnerStatement(), nest.getOuterStatement());
    }

    /**
     * Get the string representation of the induction variable of a do statement.
     *
     * @param doStatement Do statement to extract the induction variable.
     * @return The string value of the induction variable. Empty string if the
     *         passed Xnode is not a FdoStatement node or has no Var node.
     */
    public static String extractInductionVariable(Xnode doStatement)
    {
        if (!Xnode.isOfCode(doStatement, Xcode.F_DO_STATEMENT))
        {
            return "";
        }
        Xnode var = doStatement.matchDirectDescendant(Xcode.VAR);
        if (var == null)
        {
            return "";
        }
        return var.value().toLowerCase();
    }

    /**
     * Compare the iteration range of two do statements.
     *
     * @param l1             First do statement.
     * @param l2             Second do statement.
     * @param withLowerBound Compare lower bound or not.
     * @return True if the iteration range are identical.
     */
    private static boolean compareIndexRanges(Xnode l1, Xnode l2, boolean withLowerBound)
    {
        // The two nodes must be do statement
        if (!Xnode.isOfCode(l1, Xcode.F_DO_STATEMENT) || !Xnode.isOfCode(l2, Xcode.F_DO_STATEMENT))
        {
            return false;
        }

        Xnode inductionVar1 = l1.matchDirectDescendant(Xcode.VAR);
        Xnode inductionVar2 = l2.matchDirectDescendant(Xcode.VAR);
        Xnode indexRange1 = l1.matchDirectDescendant(Xcode.INDEX_RANGE);
        Xnode indexRange2 = l2.matchDirectDescendant(Xcode.INDEX_RANGE);

        return inductionVar1.compareValues(inductionVar2) && Range.compare(indexRange1, indexRange2, withLowerBound);
    }

    /**
     * Compare the iteration range of two do statements.
     *
     * @param l1 First do statement.
     * @param l2 Second do statement.
     * @return True if the iteration range are identical.
     */
    public static boolean hasSameIndexRange(Xnode l1, Xnode l2)
    {
        return compareIndexRanges(l1, l2, true);
    }

    /**
     * Compare the iteration range of two do statements.
     *
     * @param l1 First do statement.
     * @param l2 Second do statement.
     * @return True if the iteration range are identical besides the lower bound.
     */
    public static boolean hasSameIndexRangeBesidesLower(Xnode l1, Xnode l2)
    {
        return compareIndexRanges(l1, l2, false);
    }

    /**
     * Create a do statement to iterate over an array from 1 to size.
     *
     * @param fbt          FbasicType of the array.
     * @param arrayName    Array name.
     * @param inductionVar Identifier of a the induction variable.
     * @param dimId        Dimension on which size is called.
     * @param xcodeml      Current translation unit.
     * @return Newly created node.
     */
    public static Xnode createDoStmtOverAssumedShapeArray(FbasicType fbt, String arrayName, String inductionVar,
            int dimId, XcodeML xcodeml)
    {
        // Induction variable
        Xnode induction = xcodeml.createVar(FortranType.INTEGER, inductionVar, Xscope.LOCAL);

        // Lower bound
        Xnode lb = xcodeml.createNode(Xcode.LOWER_BOUND);
        lb.append(xcodeml.createIntConstant(1));

        // upper bound
        Xnode up = xcodeml.createNode(Xcode.UPPER_BOUND);
        FunctionCall sizeCall = xcodeml.createIntrinsicFctCall(FortranType.INTEGER, Xintrinsic.SIZE);
        sizeCall.addArguments(xcodeml.createVar(fbt.getType(), arrayName, Xscope.LOCAL));
        sizeCall.addArguments(xcodeml.createIntConstant(dimId));
        up.append(sizeCall);

        // step
        Xnode step = xcodeml.createNode(Xcode.STEP);
        step.append(xcodeml.createIntConstant(1));

        Xnode range = xcodeml.createNode(Xcode.INDEX_RANGE);
        range.append(lb);
        range.append(up);
        range.append(step);
        return xcodeml.createDoStmt(induction, range);
    }

}
