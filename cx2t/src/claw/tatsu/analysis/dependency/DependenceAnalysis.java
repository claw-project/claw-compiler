/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.analysis.dependency;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.List;

/**
 * This class hold methods to help analysis of loop dependencies on XcodeML/F
 * intermediate representation.
 *
 * @author clementval
 */
public class DependenceAnalysis
{

    private final Xnode _mainLoop;
    private DependenceDirection _directionVector;
    private Integer _distanceVector;
    private String _inductionVariable;

    /**
     * Constructs and run the analysis of dependencies.
     *
     * @param loop The do statement node to be analyzed.
     * @throws Exception If the given node is null or is not a do statement node.
     */
    public DependenceAnalysis(Xnode loop) throws Exception
    {
        _mainLoop = loop;
        analyze();
    }

    /**
     * Perform the analysis of the dependence on the given do statements.
     *
     * @throws Exception If the given node is null or is not a do statement node.
     */
    private void analyze() throws Exception
    {
        if (!Xnode.isOfCode(_mainLoop, Xcode.F_DO_STATEMENT))
        {
            throw new Exception("Analysis only on FdoStatement node");
        }

        Xnode inductionVarNode = _mainLoop.matchDirectDescendant(Xcode.VAR);
        _inductionVariable = inductionVarNode.value();

        Xnode body = _mainLoop.matchDescendant(Xcode.BODY);
        List<Xnode> arrayRefs = body.matchAll(Xcode.F_ARRAY_REF);

        _distanceVector = 0;
        _directionVector = DependenceDirection.NONE;
        for (Xnode arrayRef : arrayRefs)
        {
            List<Xnode> arrayIndexes = arrayRef.matchAll(Xcode.ARRAY_INDEX);
            for (Xnode arrayIndex : arrayIndexes)
            {
                if (Xnode.isOfCode(arrayIndex.firstChild(), Xcode.MINUS_EXPR)
                        || Xnode.isOfCode(arrayIndex.firstChild(), Xcode.PLUS_EXPR))
                {
                    Xnode expr = arrayIndex.firstChild();
                    Xnode var = expr.firstChild();
                    Xnode intConst = expr.lastChild();
                    if (var.value().endsWith(_inductionVariable))
                    {
                        _distanceVector = Integer.parseInt(intConst.value());
                        switch (arrayIndex.firstChild().opcode())
                        {
                        case MINUS_EXPR:
                            _directionVector = DependenceDirection.BACKWARD;
                            break;
                        case PLUS_EXPR:
                            _directionVector = DependenceDirection.FORWARD;
                            break;
                        default:
                            _directionVector = DependenceDirection.NONE;
                            break;
                        }
                    }
                }
            }
        }

    }

    /**
     * Get the induction variable string value of the iteration space.
     *
     * @return String value representing the induction variable.
     */
    public String getInductionVariable()
    {
        return _inductionVariable;
    }

    /**
     * Get the distance vector. Represents the "shape" of the dependence.
     *
     * @return Integer value representing the distance vector.
     */
    public int getDistanceVector()
    {
        return _distanceVector;
    }

    /**
     * Get the direction vector. Represents the direction in which the dependence is
     * defined.
     *
     * @return Enumeration value representing the direction (none, backward,
     *         forward)
     */
    public DependenceDirection getDirectionVector()
    {
        return _directionVector;
    }

    /**
     * Check whether the iteration space has dependencies or not.
     *
     * @return True if the iteration space is independent. False otherwise.
     */
    public boolean isIndependent()
    {
        return _directionVector == DependenceDirection.NONE && _distanceVector == 0;
    }

    /**
     * Get the do statement node used for this analysis.
     *
     * @return The node.
     */
    public Xnode getDoStmt()
    {
        return _mainLoop;
    }

    /**
     * Get a formatted message that holds various information.
     *
     * @return Message containing the line number of the do statement, the
     *         information about the dependence and the induction variable.
     */
    public String getInfoMsg()
    {
        String msg = isIndependent() ? ", Loop is parallelizable over "
                : (_directionVector == DependenceDirection.BACKWARD) ? ", Loop carried backward dependence over "
                        : ", Loop carried forward dependence over ";
        return _mainLoop.lineNo() + msg + getInductionVariable();
    }
}
