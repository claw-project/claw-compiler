/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

/**
 * The XstructType class represents the FstructType element in XcodeML.
 *
 * @author clementval
 */
public class XstructType extends Xnode {

  /**
   * Basic ctor from Xnode.
   *
   * @param node Xnode object.
   */
  public XstructType(Xnode node) {
    this(node.element());
  }

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XstructType(Element baseElement) {
    super(baseElement);
  }

  /**
   * Check is the struct type is an extension of another type.
   *
   * @return True if the struct type is an extension. False otherwise.
   */
  public boolean isExtend() {
    return hasAttribute(Xattr.EXTEND);
  }

  /**
   * Get the value of the extend attribute.
   *
   * @return String value of the extend attribute.
   */
  public String getExtend() {
    return getAttribute(Xattr.EXTEND);
  }
}
