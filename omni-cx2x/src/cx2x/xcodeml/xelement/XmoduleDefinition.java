/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.helper.XelementHelper;
import org.w3c.dom.Element;

/**
 * The XmoduleDefinition represents the FmoduleDefinition (5.7) element in XcodeML
 * intermediate representation.
 *
 * Elements: (symbols?, declarations?, FcontainsStatement?)
 * - Optional:
 *   - symbols (XsymbolTable)
 *   - declarations  (XdeclTable)
 *   - FcontainsStatement // TODO
 *
 * Attributes:
 * - Required: name (text)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */
public class XmoduleDefinition extends XenhancedElement {

  private String _name;
  private XsymbolTable _symbols;
  private XdeclTable _declarations;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XmoduleDefinition(Element baseElement){
    super(baseElement);

    _name = XelementHelper.getAttributeValue(this, XelementName.ATTR_NAME);
    _symbols = XelementHelper.findSymbols(this, false);
    _declarations = XelementHelper.findDeclarations(this, false);
  }

  /**
   * Get module name.
   * @return Module name.
   */
  public String getName(){
    return _name;
  }


}
