/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.helper.XnodeUtil;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.List;

/**
 * XcodeML AST node.
 *
 * @author clementval
 */
public class Xnode {

  public static final int LHS = 0;
  public static final int RHS = 1;
  final Element _baseElement;
  private boolean _isDeleted = false;

  /**
   * Constructs an Xnode object from an element in the AST.
   *
   * @param element Base element for the Xnode object.
   */
  public Xnode(Element element) {
    _baseElement = element;
  }

  /**
   * Constructs a new element in the AST.
   *
   * @param opcode  Code of the new element.
   * @param xcodeml Current XcodeML file unit in which the element is
   *                created.
   */
  public Xnode(Xcode opcode, XcodeML xcodeml) {
    _baseElement = xcodeml.getDocument().createElement(opcode.code());
  }

  /**
   * Get the element opcode.
   *
   * @return Opcode.
   */
  public Xcode opcode() {
    return Xcode.valueOf(_baseElement.getTagName().toUpperCase());
  }

  /**
   * Check whether the element has the corresponding attribute.
   *
   * @param attrCode Attribute code.
   * @return True if the element has the corresponding attribute.
   */
  public boolean hasAttribute(Xattr attrCode) {
    return hasAttribute(attrCode.toString());
  }

  /**
   * Check whether the element has the corresponding attribute.
   *
   * @param attrCode Attribute code.
   * @return True if the element has the corresponding attribute.
   */
  public boolean hasAttribute(String attrCode) {
    return _baseElement != null
        && _baseElement.hasAttribute(attrCode);
  }

  /**
   * Get the attribute's value.
   *
   * @param attrCode Attribute code.
   * @return Attribute's value.
   */
  public String getAttribute(Xattr attrCode) {
    return getAttribute(attrCode.toString());
  }

  /**
   * Get the attribute's value.
   *
   * @param attrCode Attribute code.
   * @return Attribute's value.
   */
  public String getAttribute(String attrCode) {
    if(_baseElement.hasAttribute(attrCode)) {
      return _baseElement.getAttribute(attrCode);
    } else {
      return null;
    }
  }

  /**
   * Get boolean value of an attribute.
   *
   * @param attrCode Attribute code.
   * @return Attribute's value. False if attribute doesn't exist.
   */
  public boolean getBooleanAttribute(Xattr attrCode) {
    return getBooleanAttribute(attrCode.toString());
  }

  /**
   * Get boolean value of an attribute.
   *
   * @param attrCode Attribute code.
   * @return Attribute's value. False if attribute doesn't exist.
   */
  public boolean getBooleanAttribute(String attrCode) {
    return _baseElement.hasAttribute(attrCode) &&
        _baseElement.getAttribute(attrCode).equals(Xname.TRUE);
  }

  /**
   * Get the element's value.
   *
   * @return Element value.
   */
  public String getValue() {
    return _baseElement.getTextContent().trim();
  }

  /**
   * Set the element value.
   *
   * @param value The element value.
   */
  public void setValue(String value) {
    _baseElement.setTextContent(value);
  }

  /**
   * Set attribute value.
   *
   * @param attrCode Attribute code.
   * @param value    Value of the attribute.
   */
  public void setAttribute(Xattr attrCode, String value) {
    setAttribute(attrCode.toString(), value);
  }

  /**
   * Set attribute value.
   *
   * @param attrCode Attribute code.
   * @param value    Value of the attribute.
   */
  public void setAttribute(String attrCode, String value) {
    _baseElement.setAttribute(attrCode, value);
  }

  /**
   * Get the list of child elements.
   *
   * @return List of children of the current element.
   */
  public List<Xnode> getChildren() {
    List<Xnode> nodes = new ArrayList<>();
    NodeList children = _baseElement.getChildNodes();
    for(int i = 0; i < children.getLength(); ++i) {
      Node child = children.item(i);
      if(child.getNodeType() == Node.ELEMENT_NODE) {
        nodes.add(new Xnode((Element) child));
      }
    }
    return nodes;
  }

  /**
   * Check whether the current element has a body element.
   *
   * @return True if the element has a body. False otherwise.
   */
  public boolean hasBody() {
    switch(opcode()) {
      case FDOSTATEMENT:
      case FFUNCTIONDEFINITION:
        return true;
    }
    return false;
  }

  /**
   * Get the inner body element.
   *
   * @return The body element if found. Null otherwise.
   */
  public Xnode getBody() {
    return findNode(Xcode.BODY);
  }

  /**
   * Find first child with the given opcode.
   *
   * @param opcode Code of the element to be found.
   * @return The found element. Null if nothing found.
   */
  public Xnode findNode(Xcode opcode) {
    List<Xnode> children = getChildren();
    for(Xnode child : children) {
      if(child.opcode() == opcode) {
        return child;
      }
    }
    return null;
  }

  /**
   * Match any first child with opcode in the given list.
   *
   * @param opcodes List of opcodes to be searched.
   * @return Matched element. Null if nothing matches.
   */
  public Xnode matchAny(List<Xcode> opcodes) {
    List<Xnode> children = getChildren();
    for(Xnode child : children) {
      if(opcodes.contains(child.opcode())) {
        return child;
      }
    }
    return null;
  }

  /**
   * Find a specific element in the children of the current element.
   *
   * @param opcodes List of opcode to reach the element.
   * @return The element if found. Null otherwise.
   */
  public Xnode find(Xcode... opcodes) {
    Xnode tmp = this;
    for(Xcode opcode : opcodes) {
      tmp = tmp.findNode(opcode);
      if(tmp == null) {
        return null;
      }
    }
    return tmp;
  }

  /**
   * Get child at position.
   *
   * @param pos Position of the child.
   * @return Child at the corresponding position.
   */
  public Xnode getChild(int pos) {
    List<Xnode> children = getChildren();
    if(pos < 0 || pos > children.size() - 1) {
      return null;
    }
    return children.get(pos);
  }

  /**
   * Get the first child node.
   *
   * @return First child or null if no child exists.
   */
  public Xnode getFirstChild() {
    List<Xnode> children = this.getChildren();
    return children.size() == 0 ? null : children.get(0);
  }

  /**
   * Get the last child node.
   *
   * @return Last child or null if no child exists.
   */
  public Xnode getLastChild() {
    List<Xnode> children = this.getChildren();
    return children.size() == 0 ? null : children.get(children.size() - 1);
  }

  /**
   * Get the number of children node in the current node.
   *
   * @return Number of children.
   */
  public int size() {
    return this.getChildren().size();
  }

  /**
   * Delete the stored root element and all its children.
   */
  public void delete() {
    _isDeleted = true;
    XnodeUtil.delete(_baseElement);
  }

  /**
   * Check whether the node has been deleted.
   *
   * @return True if the node has been deleted. False otherwise.
   */
  public boolean isDeleted() {
    return _isDeleted;
  }

  /**
   * Get the base element.
   *
   * @return Element.
   */
  public Element getElement() {
    return _baseElement;
  }

  /**
   * Create an identical copy of the element and its children.
   *
   * @return A node representing the root element of the clone.
   */
  public Node cloneNode() {
    return _baseElement.cloneNode(true);
  }

  /**
   * Append an element ot the children of this element.
   *
   * @param node  The element to append.
   * @param clone If true, the element is cloned before being appended. If
   *              false, the element is directly appended.
   */
  public void appendToChildren(Xnode node, boolean clone) {
    if(node != null) {
      if(clone) {
        _baseElement.appendChild(node.cloneNode());
      } else {
        _baseElement.appendChild(node.getElement());
      }
    }
  }

  /**
   * Insert as first child.
   *
   * @param node  Element to be inserted.
   * @param clone Clone or not the element before insertion.
   */
  public void insert(Xnode node, boolean clone) {
    if(node != null) {
      NodeList children = _baseElement.getChildNodes();
      Node toInsert = clone ? node.cloneNode() : node.getElement();
      if(children.getLength() == 0) {
        _baseElement.appendChild(toInsert);
      } else {
        _baseElement.insertBefore(toInsert, children.item(0));
      }
    }
  }

  /**
   * Set the lineno attribute in the element.
   *
   * @param lineno Line number.
   */
  public void setLine(int lineno) {
    setAttribute(Xattr.LINENO, String.valueOf(lineno));
  }

  /**
   * Clone the current element with all its children.
   *
   * @return A copy of the current element.
   */
  public Xnode cloneObject() {
    Node clone = cloneNode();
    return new Xnode((Element) clone);
  }

  /**
   * Get the lineno attribute value. This attribute is not defined for every
   * elements.
   *
   * @return Line number. 0 if the attribute is not defined.
   */
  public int getLineNo() {
    if(_baseElement.hasAttribute(Xattr.LINENO.toString())) {
      return Integer.parseInt(
          _baseElement.getAttribute(Xattr.LINENO.toString())
      );
    } else {
      return 0;
    }
  }

  /**
   * Get the file attribute value. This attribute is not defined for every
   * elements.
   *
   * @return File path. Null if the file attribute is not defined.
   */
  public String getFile() {
    return (_baseElement.hasAttribute(Xattr.FILE.toString())) ?
        _baseElement.getAttribute(Xattr.FILE.toString()) : null;
  }

  /**
   * Set the file attribute of the element.
   *
   * @param value File path.
   */
  public void setFile(String value) {
    setAttribute(Xattr.FILE, value);
  }

}
