/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

/**
 * The XmoduleDefinition represents the FmoduleDefinition (5.7) element in
 * XcodeML intermediate representation.
 * <p>
 * Elements: (symbols?, declarations?, FcontainsStatement?)
 * - Optional:
 * - symbols (XsymbolTable)
 * - declarations  (XdeclTable)
 * - FcontainsStatement (Xnode)
 * <p>
 * Attributes:
 * - Required: name (text)
 * <p>
 * Can have lineno and file attributes
 *
 * @author clementval
 */
public class XmoduleDefinition extends Xnode {

  private final String _name;
  private final XsymbolTable _symbols;
  private final XdeclTable _declarations;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XmoduleDefinition(Element baseElement) {
    super(baseElement);
    _name = getAttribute(Xattr.NAME);
    Xnode symbols = matchSeq(Xcode.SYMBOLS);
    _symbols = (symbols != null) ? new XsymbolTable(symbols.element()) : null;
    Xnode declarations = matchSeq(Xcode.DECLARATIONS);
    _declarations = (declarations != null) ?
        new XdeclTable(declarations.element()) : null;
  }

  /**
   * Get module name.
   *
   * @return Module name.
   */
  public String getName() {
    return _name;
  }


  /**
   * Get the module's symbols table.
   *
   * @return A XsymbolTable object containing the module's symbols.
   */
  public XsymbolTable getSymbolTable() {
    return _symbols;
  }

  /**
   * Get the module's declarations table.
   *
   * @return A XdeclTable object containing the module's declarations.
   */
  public XdeclTable getDeclarationTable() {
    return _declarations;
  }

  @Override
  public XmoduleDefinition cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new XmoduleDefinition(clone);
  }
}
