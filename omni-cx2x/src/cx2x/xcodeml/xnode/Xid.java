/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xid represents the id (8.2) element in XcodeML intermediate
 * representation.
 * <p>
 * Elements:
 * - Required:
 * - name (Xname)
 * Attributes:
 * - Required: type (text), sclass (text: auto, param, extern, extern_def,
 * label, tagname) // TODO move to enum
 *
 * @author clementval
 */

public class Xid extends Xnode {

  private Xnode _xname;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public Xid(Element baseElement) {
    super(baseElement);
    _xname = matchSeq(Xcode.NAME);
  }

  /**
   * Get the id name value.
   *
   * @return Name value.
   */
  public String getName() {
    return (_xname != null) ? _xname.value() : null;
  }

  /**
   * Set id name value.
   *
   * @param value The new name value.
   */
  public void setName(String value) {
    if(_xname != null && value != null) {
      _xname.setValue(value);
    }
  }

  /**
   * Set id type value.
   *
   * @param value The new type value.
   */
  public void setType(String value) {
    if(_baseElement != null && value != null) {
      _baseElement.setAttribute(Xname.ATTR_TYPE, value);
    }
  }

  /**
   * Get the sclass value.
   *
   * @return Sclass value.
   */
  public String getSclass() {
    return getAttribute(Xattr.SCLASS);
  }

  /**
   * Set id sclass value.
   *
   * @param value The new sclass value.
   */
  public void setSclass(String value) {
    if(_baseElement != null && value != null) {
      _baseElement.setAttribute(Xname.ATTR_SCLASS, value);
    }
  }

  /**
   * @return A new object Xid that is the clone of the current object.
   */
  public Xid cloneNode() {
    Node clone = cloneRawNode();
    return new Xid((Element) clone);
  }

  /**
   * Return a brief description of the Xid.
   *
   * @return String description of the Xid as
   * "name-value (type=type-value, sclass=sclass-value)".
   */
  @Override
  public String toString() {
    return String.format("%s (type=%s, sclass=%s)", getName(),
        getType(), getSclass());
  }

}
