/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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
  private final Xnode _body;
  private final Xnode _name;

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
    _symbolTable = new XsymbolTable(symbols.getElement());
    Xnode declarations = matchSeq(Xcode.DECLARATIONS);
    assert (declarations != null);
    _declTable = new XdeclTable(declarations.getElement());
    _params = matchSeq(Xcode.PARAMS);
    _body = matchSeq(Xcode.BODY);
    _name = matchSeq(Xcode.NAME);
    assert (_name != null);
    assert (_body != null);
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
   * Get the function's body.
   *
   * @return A Xbody object for the function.
   */
  public Xnode getBody() {
    return _body;
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
   * Create an identical copy of the current function definition.
   *
   * @return A new XfunctionDefinition object that is the clone of this function definition.
   */
  public XfunctionDefinition cloneObject() {
    Element clone = (Element) cloneNode();
    return new XfunctionDefinition(clone);
  }
}
