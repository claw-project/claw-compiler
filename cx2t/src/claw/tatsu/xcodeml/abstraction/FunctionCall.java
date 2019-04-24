/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.Xintrinsic;

import java.util.Collections;
import java.util.List;

/**
 * @author clementval
 */
public class FunctionCall extends Xnode {

  private final String _name;
  private final Xnode _arguments;
  private final boolean _isTbp;

  /**
   * Basic ctor from Xnode.
   *
   * @param node Raw node.
   */
  public FunctionCall(Xnode node) {
    super(node == null ? null : node.element());

    _name = getFctNameFromFctCall();
    _isTbp = Xnode.isOfCode(firstChild(), Xcode.F_MEMBER_REF);
    _arguments = matchDirectDescendant(Xcode.ARGUMENTS);
  }

  /**
   * Get called function name.
   *
   * @return Function call called.
   */
  public String getFctName() {
    return _name;
  }

  /**
   * Get list of arguments of the function call.
   *
   * @return List of arguments nodes.
   */
  public List<Xnode> arguments() {
    return _arguments != null ? _arguments.children() : Collections.emptyList();
  }

  /**
   * Check whether the call is a call to a type-bound procedure.
   * 
   * @return True if the call is a type-bound procedure call.
   */
  public boolean isTbpCall() {
    return _isTbp;
  }

  public void addArguments(Xnode newArg) {
    _arguments.append(newArg);
  }

  /**
   * Extract the name of the function in a function call.
   *
   * @return Function name if can be extracted. Null otherwise.
   */
  private String getFctNameFromFctCall() {
    if(Xnode.isOfCode(firstChild(), Xcode.F_MEMBER_REF)) {
      return firstChild().getAttribute(Xattr.MEMBER);
    } else {
      return matchSeq(Xcode.NAME).value();
    }
  }

  /**
   * Check if the function call is an intrinsic call of the given type.
   *
   * @param intrinsic Intrinsic to be checked for.
   * @return True if the function call is an intrinsic call of the given
   * intrinsic. False otherwise.
   */
  public boolean isIntrinsicCall(Xintrinsic intrinsic) {
    if(!getBooleanAttribute(Xattr.IS_INTRINSIC)) {
      return false;
    }
    return getFctName().equalsIgnoreCase(intrinsic.toString());
  }

}
