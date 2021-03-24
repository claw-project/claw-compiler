/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import org.w3c.dom.Element;

import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Abstraction of assignment statement to be able to categorize them and easily
 * extract some information.
 *
 * @author clementval
 */
public class AssignStatement extends Xnode
{

    /**
     * Constructs an Xnode object from an element in the AST.
     *
     * @param element Base element for the Xnode object.
     */
    public AssignStatement(Element element)
    {
        super(element);
    }

    /**
     * Get the variable name on the left hand-side of the assignment.
     *
     * @return Left hand-side variable name.
     */
    public String getLhsName()
    {
        Xnode lhs = getLhs();
        if (lhs != null)
        {
            return lhs.is(Xcode.VAR) ? lhs.value() : lhs.matchSeq(Xcode.VAR_REF, Xcode.VAR).value();
        }
        return "";
    }

    /**
     * Get the left hand-side node of the assignment.
     *
     * @return Left hans-side node.
     */
    public Xnode getLhs()
    {
        return child(Xnode.LHS);
    }

    /**
     * Get the right hand-side node of the assignment.
     *
     * @return Right hans-side node.
     */
    public Xnode getRhs()
    {
        return child(Xnode.RHS);
    }

    /**
     * Check if the current assignment statement is a child of a give type of node.
     *
     * @param opcode Opcode of the ancestor to check for.
     * @return True if on of the ancestor is of the given kind. Search is contained
     *         in the function definition itself.
     */
    public boolean isChildOf(Xcode opcode)
    {
        Xnode crt = this;
        while (crt != null)
        {
            if (Xnode.isOfCode(crt.ancestor(), opcode))
            {
                return true;
            }
            // Stop searching when FfunctionDefinition is reached
            if (Xnode.isOfCode(crt.ancestor(), Xcode.F_FUNCTION_DEFINITION))
            {
                return false;
            }
            crt = crt.ancestor();
        }
        return false;
    }

    /**
     * Get list of array variable names used in assignment statement.
     *
     * @return List of variables.
     */
    public Set<String> getVarNames()
    {
        return filterVars(matchAll(Xcode.VAR));
    }

    /**
     * Get all variables names on the RHS.
     *
     * @return Set of variables names used on the RHS.
     */
    public Set<String> getReadNames()
    {
        return filterVars(getRhs().matchAll(Xcode.VAR));
    }

    /**
     * Filter a list of Var nodes to keep only the real var and exclude the array
     * index variables.
     *
     * @param vars List of Var node to filter.
     * @return Set of variable names.
     */
    private Set<String> filterVars(List<Xnode> vars)
    {
        return vars.stream().filter(Xnode::isNotArrayIndex).map(Xnode::value).collect(Collectors.toSet());
    }

    /**
     * Check whether the assignment is made with constant only.
     *
     * @return True of the assignment is constant only. False otherwise.
     */
    public boolean isConstantAssignement()
    {
        Set<String> usedVars = getReadNames();
        usedVars.remove(getLhsName());
        return Xnode.isOfCode(getRhs(), Xcode.F_INT_CONSTANT) || Xnode.isOfCode(getLhs(), Xcode.F_REAL_CONSTANT)
                || usedVars.isEmpty();
    }
}
