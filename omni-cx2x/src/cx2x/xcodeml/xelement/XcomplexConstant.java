/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import java.util.List;
import cx2x.xcodeml.helper.*;

/**
 * The XcomplexConstant represents the FcomplexConstant elements (7.1.2) element
 * in XcodeML intermediate representation.
 *
 * Elements: ( FrealConstant, FrealConstant )
 * - Required: FrealConstant (XrealConstant)
 * Attributes: defined in Xconstant
 * - Optional: type (text)
 *
 * @author clementval
 */

public class XcomplexConstant extends Xconstant {

  private String _type = null;

  private XrealConstant _realConst1 = null;
  private XrealConstant _realConst2 = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XcomplexConstant(Element baseElement){
    super(baseElement);
    readElementInformation();
    _type = XelementHelper.getAttributeValue(this, XelementName.ATTR_TYPE);
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    List<XrealConstant> innerElements = XelementHelper
      .getRealConstants(this);
    if(innerElements.size() == 2){
      _realConst1 = innerElements.get(0);
      _realConst2 = innerElements.get(1);
    }
  }

  /**
   * Get the real part of the constant
   * @return A XrealConstant object representing the real part.
   */
  public XrealConstant getRealPart(){
    return _realConst1;
  }

  /**
   * Get the imaginary part of the constant
   * @return A XrealConstant object representing the imaginary part.
   */
  public XrealConstant getImaginaryPart(){
    return _realConst2;
  }

  /**
   * Get the type value.
   * @return Type value.
   */
  public String getType(){
    return _type;
  }
}
