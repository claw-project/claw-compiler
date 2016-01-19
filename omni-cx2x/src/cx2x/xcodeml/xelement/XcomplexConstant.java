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
 * Elements:
 * - Required: FrealConstant (XrealConstant)
 * Attributes: defined in Xconstant
 * - Optional: type (text)
 *
 * @author clementval
 */

public class XcomplexConstant extends Xconstant {
  XrealConstant realConst1 = null;
  XrealConstant realConst2 = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XcomplexConstant(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    List<XrealConstant> innerElements = XelementHelper
      .getRealConstants(this);
    if(innerElements.size() != 2){
      // TODO error handling : exception
    }
    realConst1 = innerElements.get(0);
    realConst2 = innerElements.get(1);
  }
}
