/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XexprStatement represents the FpragmaStatement (6.2) element in XcodeML
 * intermediate representation.
 *
 * Elements: exprModel
 *
 * @author clementval
 */

public class XexprStatement extends XbaseElement
    implements Xclonable<XexprStatement>
{

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XexprStatement(Element baseElement){
    super(baseElement);
  }

  @Override
  public XexprStatement cloneObject() {
    Element clone = (Element)cloneNode();
    return new XexprStatement(clone);
  }
}
