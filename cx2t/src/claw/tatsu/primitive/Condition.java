/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.Set;

/**
 * Primitive analysis and transformation on condition node. This included: -
 * Check condition dependency on certain variables.
 *
 * @author clementval
 */
public final class Condition
{

    // Avoid instantiation of this class
    private Condition()
    {
    }

    /**
     * Check whether the condition depends on some variables.
     *
     * @param condition Condition element to check.
     * @param variables List of variable name.
     * @return True if the condition depend on a variable in the list. False
     *         otherwise.
     */
    public static boolean dependsOn(Xnode condition, Set<String> variables)
    {
        if (!Xnode.isOfCode(condition, Xcode.CONDITION) || variables.isEmpty())
        {
            return false;
        }
        return condition.matchAll(Xcode.VAR).stream()
                .anyMatch(v -> v.isNotArrayIndex() && variables.contains(v.value()));
    }

    /**
     * Check if the condition is checking if a variable is allocated.
     *
     * @param condition Condition node to check.
     * @return True if the condition is using the allocated intrinsic. False
     *         otherwise.
     */
    public static boolean isAllocationRelated(Xnode condition)
    {
        if (!Xnode.isOfCode(condition, Xcode.CONDITION))
        {
            return false;
        }
        return condition.matchAll(Xcode.FUNCTION_CALL).stream().map(FunctionCall::new).map(FunctionCall::getFctName)
                .anyMatch(Xname.F_INTR_ALLOCATED::equalsIgnoreCase);
    }

}
