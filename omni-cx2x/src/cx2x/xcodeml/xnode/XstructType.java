/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

/**
 * The XstructType class represents the FstructType (3.5) element in XcodeML/F.
 * <p>
 * Elements: (params?)
 * - Optional: typeParams, symbols, typeBoundProcedures
 * <p>
 * Attributes:
 * - Required: type (text)
 * - Optional: is_public (bool), is_private (bool), is_sequence (bool),
 * is_internal_private (bool), is_abstract (bool), extends (text),
 * bind (text)
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
    return hasAttribute(Xattr.EXTENDS);
  }

  /**
   * Get the value of the extend attribute.
   *
   * @return String value of the extend attribute.
   */
  public String getExtend() {
    return getAttribute(Xattr.EXTENDS);
  }

  /**
   * Get the value of the bind attribute.
   *
   * @return String value of the bind attribute.
   */
  public String getBind() {
    return getAttribute(Xattr.BIND);
  }

  /**
   * Check whether the type is public.
   *
   * @return True if the attribute is_public is set to true. False otherwise.
   */
  public boolean isPublic() {
    return getBooleanAttribute(Xattr.IS_PUBLIC);
  }

  /**
   * Check whether the type is private.
   *
   * @return True if the attribute is_private is set to true. False otherwise.
   */
  public boolean isPrivate() {
    return getBooleanAttribute(Xattr.IS_PRIVATE);
  }

  /**
   * Check whether the type is sequence.
   *
   * @return True if the attribute is_sequence is set to true. False otherwise.
   */
  public boolean isSequence() {
    return getBooleanAttribute(Xattr.IS_SEQUENCE);
  }

  /**
   * Check whether the type is internal private.
   *
   * @return True if the attribute is_internal_private is set to true. False
   * otherwise.
   */
  public boolean isInternalPrivate() {
    return getBooleanAttribute(Xattr.IS_INTERNAL_PRIVATE);
  }

  /**
   * Check whether the type is abstract.
   *
   * @return True if the attribute is_abstract is set to true. False otherwise.
   */
  public boolean isAbstract() {
    return getBooleanAttribute(Xattr.IS_ABSTRACT);
  }
}
