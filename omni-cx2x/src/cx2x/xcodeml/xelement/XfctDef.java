/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The XfctDef represents the FfunctionDefinition (5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - name (text)
 *   - body
 * - Optional:
 *   - symbols (XsymbolTable)
 *   - params
 *   - declarations (XdeclTable)
 *
 * @author clementval
 */

public class XfctDef extends Xfct implements Xclonable<XfctDef> {

  private XsymbolTable _symbolTable = null;
  private XdeclTable _declTable = null;
  private Xbody _body = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XfctDef(Element baseElement){
    super(baseElement);
    _symbolTable = XelementHelper.findSymbols(this, false);
    _declTable = XelementHelper.findDeclarations(this, false);
    _body = XelementHelper.findBody(this, false);
  }

  /**
   * Get the function's symbols table.
   * @return A XsymbolTable object containing the function's symbols.
   */
  public XsymbolTable getSymbolTable(){
    return _symbolTable;
  }

  /**
   * Get the function's declarations table.
   * @return A XdeclTable object containing the function's declarations.
   */
  public XdeclTable getDeclarationTable(){
    return _declTable;
  }

  /**
   * Get the function's body.
   * @return A Xbody object for the function.
   */
  public Xbody getBody(){
    return _body;
  }

  /**
   * Create an identical copy of the current function definition.
   * @return A new XfctDef object that is the clone of this function definition.
   */
  public XfctDef cloneObject(){
    Element clone = (Element)cloneNode();
    return new XfctDef(clone);
  }
}
