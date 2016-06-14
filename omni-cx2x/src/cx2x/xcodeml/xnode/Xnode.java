/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XbaseElement;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XelementName;
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

  private final Element _baseElement;

  /**
   * Constructs an Xnode object from an element in the AST.
   * @param element Base element for the Xnode object.
   */
  public Xnode(Element element){
    _baseElement = element;
  }

  /**
   * Constructs a new element in the AST.
   * @param opcode  Code of the new element.
   * @param xcodeml Current XcodeML program unit in which the element is
   *                created.
   */
  public Xnode(Xcode opcode, XcodeProgram xcodeml){
    _baseElement = xcodeml.getDocument().createElement(opcode.code());
  }

  /**
   * Get the element opcode.
   * @return Opcode.
   */
  public Xcode Opcode(){
    return Xcode.valueOf(_baseElement.getTagName().toUpperCase());
  }

  /**
   * Check whether the element has the corresponding attribute.
   * @param attrCode Attribute code.
   * @return True if the element has the corresponding attribute.
   */
  public boolean hasAttribute(Xattr attrCode){
    return _baseElement != null
        && _baseElement.hasAttribute(attrCode.toString());
  }

  /**
   * Get the attribute's value.
   * @param attrCode Attribute code.
   * @return Attribute's value.
   */
  public String getAttribute(Xattr attrCode){
    return _baseElement.getAttribute(attrCode.toString());
  }

  /**
   * Get boolean value of an attribute.
   * @param attrCode Attribute code.
   * @return Attribute's value. False if attribute doesn't exist.
   */
  public boolean getBooleanAttribute(Xattr attrCode) {
    return _baseElement.hasAttribute(attrCode.toString()) &&
        _baseElement.getAttribute(attrCode.toString()).equals(XelementName.TRUE);
  }

  /**
   * Get the element's value.
   * @return Element value.
   */
  public String getValue(){
    return _baseElement.getTextContent();
  }

  /**
   * Set attribute value.
   * @param attrCode Attribute code.
   * @param value Value of the attribute.
   */
  public void setAttribute(Xattr attrCode, String value){
    _baseElement.setAttribute(attrCode.toString(), value);
  }

  /**
   * Get the list of child elements.
   * @return List of children of the current element.
   */
  public List<Xnode> getChildren(){
    List<Xnode> nodes = new ArrayList<>();
    NodeList children = _baseElement.getChildNodes();
    for(int i = 0; i < children.getLength(); ++i){
      Node child = children.item(i);
      if(child.getNodeType() == Node.ELEMENT_NODE){
        nodes.add(new Xnode((Element)child));
      }
    }
    return nodes;
  }

  /**
   * Check whether the current element has a body element.
   * @return True if the element has a body. False otherwise.
   */
  public boolean hasBody(){
    switch (Opcode()){
      case FDOSTATEMENT:
      case FFUNCTIONDEFINITION:
        return true;
    }
    return false;
  }

  /**
   * Get the inner body element.
   * @return The body element if found. Null otherwise.
   */
  public Xnode getBody(){
    return findNode(Xcode.BODY);
  }

  /**
   * Find first child with the given opcode.
   * @param opcode Code of the element to be found.
   * @return The found element. Null if nothing found.
   */
  public Xnode findNode(Xcode opcode){
    List<Xnode> children = getChildren();
    for(Xnode child : children){
      if(child.Opcode() == opcode){
        return child;
      }
    }
    return null;
  }

  /**
   * Get child at position.
   * @param pos Position of the child.
   * @return Child at the corresponding position.
   */
  public Xnode getChild(int pos){
    List<Xnode> children = getChildren();
    if(pos < 0 || pos > children.size() - 1){
      return null;
    }
    return children.get(pos);
  }

  /**
   * Set the element value.
   * @param value The element value.
   */
  public void setValue(String value){
    _baseElement.setTextContent(value);
  }

  /**
   * Delete the stored root element and all its children.
   */
  public void delete(){
    XelementHelper.delete(_baseElement);
  }

  /**
   * Get the base element.
   * @return Element.
   */
  public Element getElement(){
    return _baseElement;
  }

  /**
   * Create an identical copy of the element and its children.
   * @return A node representing the root element of the clone.
   */
  public Node cloneNode(){
    return _baseElement.cloneNode(true);
  }

  /**
   * Append an element ot the children of this element.
   * @param node  The element to append.
   * @param clone If true, the element is cloned before being appened. If
   *              false, the element is directly appened.
   */
  public void appendToChildren(Xnode node, boolean clone){
    if(node != null){
      if(clone){
        _baseElement.appendChild(node.cloneNode());
      } else {
        _baseElement.appendChild(node.getElement());
      }
    }
  }

  /**
   * Set the file attribute of the element.
   * @param value File path.
   */
  public void setFile(String value){
    setAttribute(Xattr.FILE, value);
  }

  /**
   * Set the lineno attribute in the element.
   * @param lineno Line number.
   */
  public void setLine(int lineno){
    setAttribute(Xattr.LINENO, String.valueOf(lineno));
  }

}
