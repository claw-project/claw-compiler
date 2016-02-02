/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

/**
 * The XfunctionDefinition represents the FfunctionDefinition (5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements: (name, symbols?, params?, declarations?, body)
 * - Required:
 *   - name (text)
 *   - body (Xbody)
 * - Optional:
 *   - symbols (XsymbolTable)
 *   - params  (Xparams)
 *   - declarations (XdeclTable)
 *
 * Can have lineno and file attributes (XenhancedElement)
 *
 * @author clementval
 */

public class XfunctionDefinition extends XenhancedElement implements Xclonable<XfunctionDefinition> {

  // Elements
  private XsymbolTable _symbolTable = null;
  private Xparams _params = null;
  private XdeclTable _declTable = null;
  private Xbody _body = null;
  private Xname _name = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XfunctionDefinition(Element baseElement){
    super(baseElement);
    _symbolTable = XelementHelper.findSymbols(this, false);
    _params = XelementHelper.findParams(this, false);
    _declTable = XelementHelper.findDeclarations(this, false);
    _body = XelementHelper.findBody(this, false);
    _name = XelementHelper.findName(this, false);
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
   * Get the function name.
   * @return Name of the function as an Xname object.
   */
  public Xname getName(){
    return _name;
  }


  /**
   * Get the parameters list.
   * @return Parameters list.
   */
  public Xparams getParams(){
    return _params;
  }

  /**
   * Create an identical copy of the current function definition.
   * @return A new XfunctionDefinition object that is the clone of this function definition.
   */
  public XfunctionDefinition cloneObject(){
    Element clone = (Element)cloneNode();
    return new XfunctionDefinition(clone);
  }
}
