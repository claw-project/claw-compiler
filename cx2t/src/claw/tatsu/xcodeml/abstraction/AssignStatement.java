/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import org.w3c.dom.Element;

/**
 * Abstraction of assignment statement to be able to categorize them and
 * easily extract some information.
 *
 * @author clementval
 */
public class AssignStatement extends Xnode {

  /**
   * Constructs an Xnode object from an element in the AST.
   *
   * @param element Base element for the Xnode object.
   */
  public AssignStatement(Element element) {
    super(element);
  }

  /**
   * Get the variable name on the left hand-side of the assignment.
   *
   * @return Left hand-side variable name.
   */
  public String getLhsName() {
    Xnode lhs = getLhs();
    if(lhs != null) {
      return (lhs.opcode() == Xcode.VAR) ? lhs.value() :
          lhs.matchSeq(Xcode.VAR_REF, Xcode.VAR).value();
    }
    return "";
  }

  /**
   * Get the left hand-side node of the assignment.
   *
   * @return Left hans-side node.
   */
  public Xnode getLhs() {
    return child(Xnode.LHS);
  }

}
