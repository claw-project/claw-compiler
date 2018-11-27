/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.List;
import java.util.Set;

/**
 * Primitive analysis and transformation on condition node. This included:
 * - Check condition dependency on certain variables.
 *
 * @author clementval
 */
public final class Condition {

  // Avoid instantiation of this class
  private Condition() {
  }

  /**
   * Check whether the condition depends on some variables.
   *
   * @param condition Condition element to check.
   * @param variables List of variable name.
   * @return True if the condition depend on a variable in the list. False
   * otherwise.
   */
  public static boolean dependsOn(Xnode condition, Set<String> variables) {
    if(condition == null || condition.opcode() != Xcode.CONDITION
        || variables.isEmpty())
    {
      return false;
    }
    List<Xnode> vars = condition.matchAll(Xcode.VAR);
    for(Xnode var : vars) {
      if(var.isNotArrayIndex() && variables.contains(var.value())) {
        return true;
      }
    }
    return false;
  }


}

