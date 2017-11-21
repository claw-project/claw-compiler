/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.ArrayList;
import java.util.List;

/**
 * The XfunctionType represents the FfunctionType (3.4) element in XcodeML
 * intermediate representation.
 * <p>
 * Elements: (params?)
 * - Optional:
 * - params
 * Attributes:
 * - Required: type (text), return_type (text)
 * - Optional: result_name (text), is_recursive (bool), is_program (bool),
 * is_internal (bool), is_elemental (bool), is_pure (bool), bind (text),
 * bind_name (text)
 *
 * @author clementval
 */

public class XfunctionType extends Xnode {

  private Xnode _params = null;
  private List<Xnode> _parameters = null;

  /**
   * Basic ctor from Xnode.
   *
   * @param node Raw node.
   */
  public XfunctionType(Xnode node) {
    super(node == null ? null : node.element());
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation() {
    _params = matchSeq(Xcode.PARAMS);
    if(_params != null) {
      _parameters = _params.matchAll(Xcode.NAME);
    } else {
      _parameters = new ArrayList<>();
    }
  }

  /**
   * Get the function result name.
   *
   * @return Result name value.
   */
  public String getResultName() {
    return getAttribute(Xattr.RESULT_NAME);
  }

  /**
   * Check whether function is recursive.
   *
   * @return True if the function is recursive. False otherwise.
   */
  public boolean isRecursive() {
    return getBooleanAttribute(Xattr.IS_RECURSIVE);
  }

  /**
   * Check whether function is internal.
   *
   * @return True if the function is internal. False otherwise.
   */
  public boolean isInternal() {
    return getBooleanAttribute(Xattr.IS_INTERNAL);
  }

  /**
   * Get the function return type.
   *
   * @return The function's return type as String.
   */
  public String getReturnType() {
    return getAttribute(Xattr.RETURN_TYPE);
  }

  /**
   * Check whether function is the program function.
   *
   * @return True if the function is the program function. False otherwise.
   */
  public boolean isProgram() {
    return getBooleanAttribute(Xattr.IS_PROGRAM);
  }

  /**
   * Check whether function has the pure attribute set to true.
   *
   * @return True if the function has the pure attribute set to true.
   * False otherwise.
   */
  public boolean isPure() {
    return getBooleanAttribute(Xattr.IS_PURE);
  }

  /**
   * Check whether function has the elemental attribute set to true.
   *
   * @return True if the function has the elemental attribute set to true.
   * False otherwise.
   */
  public boolean isElemental() {
    return getBooleanAttribute(Xattr.IS_ELEMENTAL);
  }

  /**
   * Get the list of all parameters name node.
   *
   * @return List of parameters Xnode objects.
   */
  public List<Xnode> getParameters() {
    return _parameters;
  }

  /**
   * Add a name element to the parameters list.
   *
   * @param name The name element to add.
   */
  public void addParameters(Xnode name) {
    _parameters.add(name);
    _params.append(name);
  }

  /**
   * Add a name element to the parameters list before the referenced element.
   *
   * @param ref  Referenced element. New element will be added before.
   * @param name The name element to add.
   */
  public void addParameters(Xnode ref, Xnode name) {
    int index = _parameters.indexOf(ref);
    _parameters.add(index, name);
    ref.insertBefore(name);
  }

  /**
   * Get a list of string representing the function parameters.
   *
   * @return List of string.
   */
  public List<String> getParamsNames() {
    List<String> parameters = new ArrayList<>();
    if(_parameters == null) {
      return parameters;
    }
    for(Xnode n : _parameters) {
      parameters.add(n.value());
    }
    return parameters;
  }

  /**
   * A new object XfunctionType that is the clone of the current object.
   *
   * @return A new XfunctionType that is a clone of the current one.
   */
  public XfunctionType cloneNode() {
    return new XfunctionType(super.cloneNode());
  }

  /**
   * Check if a parameter is part of the function definition.
   *
   * @param paramName Parameter's name.
   * @return True if the parameter is found in the function definition. False
   * otherwise.
   */
  public boolean hasParam(String paramName) {
    if(_parameters == null) {
      return false;
    }
    for(Xnode param : _parameters) {
      if(param.value().toLowerCase().equals(paramName.toLowerCase())) {
        return true;
      }
    }
    return false;
  }

  @Override
  public String toString() {
    return String.format("%s type=\"%s\"", opcode(), getType());
  }
}
