/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.translator.transformation.primitive.Module;
import org.w3c.dom.Element;

/**
 * The XfunctionDefinition represents the FfunctionDefinition (5.3) element in
 * XcodeML intermediate representation.
 * <p>
 * Elements: (name, symbols?, params?, declarations?, body)
 * - Required:
 * - name (text)
 * - body (Xbody)
 * - Optional:
 * - symbols (XsymbolTable)
 * - params  (Xparams)
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
   * Xnode ctor. Delegate construction to the Element ctor.
   *
   * @param node Xnode element.
   */
  public XfunctionDefinition(Xnode node) {
    this(node.element());
  }

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XfunctionDefinition(Element baseElement) {
    super(baseElement);
    Xnode symbols = matchSeq(Xcode.SYMBOLS);
    assert (symbols != null);
    _symbolTable = new XsymbolTable(symbols.element());
    Xnode declarations = matchSeq(Xcode.DECLARATIONS);
    assert (declarations != null);
    _declTable = new XdeclTable(declarations.element());
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
  public Xnode getName() {
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
    Element clone = (Element) cloneRawNode();
    return new XfunctionDefinition(clone);
  }

}
