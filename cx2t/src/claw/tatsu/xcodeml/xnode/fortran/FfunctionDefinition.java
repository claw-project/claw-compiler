/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.primitive.Body;
import claw.tatsu.primitive.Xmod;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.*;

/**
 * The FfunctionDefinition represents the FfunctionDefinition (5.3) element in
 * XcodeML intermediate representation.
 *
 * Elements: (name, symbols?, params?, declarations?, body)
 * - Required:
 * - name (text)
 * - body
 * - Optional:
 * - symbols (XsymbolTable)
 * - params
 * - declarations (XdeclTable)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class FfunctionDefinition extends Xnode {

  // Elements
  private final XsymbolTable _symbolTable;
  private final Xnode _params;
  private final XdeclTable _declTable;
  private final Xnode _name;

  /**
   * Constructs new FfunctionDefinition instance.
   *
   * @param node Raw node.
   */
  public FfunctionDefinition(Xnode node) {
    super(node == null ? null : node.element());
    Xnode symbols = matchSeq(Xcode.SYMBOLS);
    assert (symbols != null);
    _symbolTable = new XsymbolTable(symbols);
    Xnode declarations = matchSeq(Xcode.DECLARATIONS);
    assert (declarations != null);
    _declTable = new XdeclTable(declarations);
    _params = matchSeq(Xcode.PARAMS);
    _name = matchSeq(Xcode.NAME);
    assert (_name != null);
    assert (body() != null);
  }

  /**
   * Get the function's symbols table.
   *
   * @return A XsymbolTable object containing the function's symbols.
   */
  public XsymbolTable getSymbolTable() {
    return _symbolTable;
  }

  /**
   * Get the function's declarations table.
   *
   * @return A XdeclTable object containing the function's declarations.
   */
  public XdeclTable getDeclarationTable() {
    return _declTable;
  }

  /**
   * Get the function name.
   *
   * @return Name of the function as an Xname object.
   */
  public String getName() {
    return _name.value();
  }

  /**
   * Get the name node.
   *
   * @return name node of the FfunctionDefinition.
   */
  public Xnode name() {
    return _name;
  }

  /**
   * Get the parameters list.
   *
   * @return Parameters list.
   */
  public Xnode getParams() {
    return _params;
  }

  /**
   * Find module containing the function and read its .xmod file.
   *
   * @return FortranModule object if the module has been found and read.
   * Null otherwise.
   */
  public FortranModule findContainingXmod() {
    FmoduleDefinition mod = findParentModule();
    if(mod == null) {
      return null;
    }
    return Xmod.find(mod.getAttribute(Xattr.NAME));
  }

  /**
   * Check if function body is empty.
   *
   * @return True if function body is empty.
   * False otherwise (no body or not empty).
   */
  public boolean hasEmptyBody() {
    if(body() == null) {
      return false;
    }

    try {
      return Body.isEmpty(body());
    } catch(IllegalTransformationException itex) {
      // TODO logger
      return false;
    }
  }

  /**
   * Create an identical copy of the current function definition.
   *
   * @return A new FfunctionDefinition object that is the clone of this
   * function definition.
   */
  @Override
  public FfunctionDefinition cloneNode() {
    return new FfunctionDefinition(super.cloneNode());
  }
}
