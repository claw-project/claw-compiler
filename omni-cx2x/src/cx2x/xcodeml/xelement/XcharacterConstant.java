/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XcharacterConstant represents the FcharacterConstant elements (7.1.1)
 * element in XcodeML intermediate representation.
 *
 * Elements: defined in Xconstant
 * - contains constant value
 * Attributes: defined in Xconstant
 * - Optional: type (text), kind (text)
 *
 * @author clementval
 */

public class XcharacterConstant extends Xconstant
    implements Xclonable<XcharacterConstant>
{

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XcharacterConstant(Element baseElement){
    super(baseElement);
  }


  @Override
  public XcharacterConstant cloneObject() {
    Element clone = (Element)cloneNode();
    return new XcharacterConstant(clone);
  }

}
