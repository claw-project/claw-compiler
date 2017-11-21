/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.primitive.Module;
import claw.tatsu.xcodeml.xnode.common.*;

/**
 * The XfunctionDefinition represents the FfunctionDefinition (5.3) element in
 * XcodeML intermediate representation.
 * <p>
 * Elements: (name, symbols?, params?, declarations?, body)
 * - Required:
 * - name (text)
 * - body
 * - Optional:
 * - symbols (XsymbolTable)
 * - params
 * - declarations (XdeclTable)
 * <p>
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class XfunctionDefinition extends Xnode {

  // Elements
  private final XsymbolTable _symbolTable;
  private final Xnode _params;
  private final XdeclTable _declTable;
  private final Xnode _name;

  /**
   * Constructs new XfunctionDefinition instance.
   *
   * @param node Raw node.
   */
  public XfunctionDefinition(Xnode node) {
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
   * @return Xmod object if the module has been found and read. Null otherwise.
   */
  public Xmod findContainingXmod() {
    XmoduleDefinition mod = findParentModule();
    if(mod == null) {
      return null;
    }
    return Module.find(mod.getAttribute(Xattr.NAME));
  }

  /**
   * Create an identical copy of the current function definition.
   *
   * @return A new XfunctionDefinition object that is the clone of this function definition.
   */
  @Override
  public XfunctionDefinition cloneNode() {
    return new XfunctionDefinition(super.cloneNode());
  }
}
