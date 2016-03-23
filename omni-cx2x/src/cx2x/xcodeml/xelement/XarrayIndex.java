/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import cx2x.xcodeml.helper.*;

/**
 * The XarrayIndex represents the arrayIndex (8.10) element in XcodeML
 * intermediate representation.
 *
 * Elements: (exprModel)
 * - Required:
 *   - exprModel (XbaseElement)
 *
 * @author clementval
 */

public class XarrayIndex extends Xindex {
  private XexprModel _exprModel;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XarrayIndex(Element baseElement){
    super(baseElement);
    _exprModel = XelementHelper.findExprModel(this, 0);
  }

  /**
   * Get the inner exprModel object.
   * @return The inner exprModel object.
   */
  public XexprModel getExprModel(){
    return _exprModel;
  }

  /**
   * Append a XbaseElement as the last children of XarrayIndex.
   * @param element The element to append.
   */
  public void append(XbaseElement element){
    append(element, false);
  }

  /**
   * Append a XbaseElement as the last children of XarrayIndex.
   * @param element       The element to append.
   * @param cloneElement  If true, the element is cloned before being added. If
   *                      false, the element is directly added.
   */
  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.cloneNode();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }
  }

}
