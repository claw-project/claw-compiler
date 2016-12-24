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
  public static final int UNDEF_DEPTH = -1;
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
  public String value() {
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
   * Check whether the current element has a body element.
   *
   * @return True if the element has a body. False otherwise.
   */
  public boolean hasBody() {
    switch(opcode()) {
      case FDOSTATEMENT:
      case FFUNCTIONDEFINITION:
      case FDOWHILESTATEMENT:
      case FCASELABEL:
      case THEN:
      case ELSE:
      case TYPEGUARD:
        return true;
    }
    return false;
  }

  /**
   * Get the inner body element.
   *
   * @return The body element if found. Null otherwise.
   */
  public Xnode body() {
    return matchDirectDescendant(Xcode.BODY);
  }

  /**
   * Get child at position.
   *
   * @param pos Position of the child.
   * @return Child at the corresponding position.
   */
  public Xnode child(int pos) {
    List<Xnode> children = children();
    if(pos < 0 || pos > children.size() - 1) {
      return null;
    }
    return children.get(pos);
  }

  /**
   * Get the list of child elements.
   *
   * @return List of children of the current element.
   */
  public List<Xnode> children() {
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
   * Get the first child node.
   *
   * @return First child or null if no child exists.
   */
  public Xnode firstChild() {
    List<Xnode> children = this.children();
    return children.size() == 0 ? null : children.get(0);
  }

  /**
   * Get the last child node.
   *
   * @return Last child or null if no child exists.
   */
  public Xnode lastChild() {
    List<Xnode> children = this.children();
    return children.size() == 0 ? null : children.get(children.size() - 1);
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
  public Element element() {
    return _baseElement;
  }

  /**
   * Append an element ot the children of this element.
   *
   * @param node  The element to append.
   * @param clone If true, the element is cloned before being appended. If
   *              false, the element is directly appended.
   */
  public void append(Xnode node, boolean clone) {
    if(node != null) {
      if(clone) {
        _baseElement.appendChild(node.cloneRawNode());
      } else {
        _baseElement.appendChild(node.element());
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
      Node toInsert = clone ? node.cloneRawNode() : node.element();
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
   * Get the lineno attribute value. This attribute is not defined for every
   * elements.
   *
   * @return Line number. 0 if the attribute is not defined.
   */
  public int lineNo() {
    if(_baseElement.hasAttribute(Xattr.LINENO.toString())) {
      return Integer.parseInt(
          _baseElement.getAttribute(Xattr.LINENO.toString())
      );
    } else {
      return 0;
    }
  }

  /**
   * Get the file attribute value. This value represents the original filename
   * from the XcodeML unit. This attribute is not defined for every elements.
   *
   * @return File path. Null if the file attribute is not defined.
   */
  public String filename() {
    return (_baseElement.hasAttribute(Xattr.FILE.toString())) ?
        _baseElement.getAttribute(Xattr.FILE.toString()) : null;
  }

  /**
   * Set the file attribute of the element.
   *
   * @param value File path.
   */
  public void setFilename(String value) {
    setAttribute(Xattr.FILE, value);
  }

  /**
   * Clone the current element with all its children.
   *
   * @return A copy of the current element.
   */
  public Xnode cloneNode() {
    Node clone = cloneRawNode();
    return new Xnode((Element) clone);
  }

  /**
   * Create an identical copy of the element and its children.
   *
   * @return A node representing the root element of the clone.
   */
  public Node cloneRawNode() {
    return _baseElement.cloneNode(true);
  }


  /**
   * Get next sibling node.
   *
   * @return Next sibling node.
   */
  public Xnode nextSibling() {
    if(_baseElement == null) {
      return null;
    }
    Node n = _baseElement.getNextSibling();
    while(n != null) {
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        return new Xnode((Element) n);
      }
      n = n.getNextSibling();
    }
    return null;
  }

  /**
   * Find node with the given opcode in the ancestors of the current node.
   *
   * @param opcode Opcode of the node to be matched.
   * @return The matched node. Null if nothing matched.
   */
  public Xnode matchAncestor(Xcode opcode) {
    return universalMatch(opcode, false);
  }

  /**
   * Find node with the given opcode in the siblings of the given node.
   *
   * @param opcode Opcode of the node to be matched.
   * @return The matched node. Null if no node found.
   */
  public Xnode matchSibling(Xcode opcode) {
    return universalMatch(opcode, true);
  }

  /**
   * Find node with the given opcode in the descendants of the current node.
   *
   * @param opcode Opcode of the node to be matched.
   * @return The matched node. Null if nothing matched.
   */
  public Xnode matchDescendant(Xcode opcode)
  {
    if(_baseElement == null) {
      return null;
    }
    NodeList elements = _baseElement.getElementsByTagName(opcode.code());
    if(elements.getLength() == 0) {
      return null;
    }
    return (elements.item(0) == null) ? null :
        new Xnode((Element) elements.item(0));
  }

  /**
   * Find node with the given opcode in the direct descendants the current node.
   *
   * @param opcode Opcode of the node to be matched.
   * @return The matched node. Null if nothing matched.
   */
  public Xnode matchDirectDescendant(Xcode opcode) {
    if(_baseElement == null) {
      return null;
    }
    NodeList nodeList = _baseElement.getChildNodes();
    for(int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if(nextNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) nextNode;
        if(element.getTagName().equals(opcode.code())) {
          return new Xnode(element);
        }
      }
    }
    return null;
  }

  /**
   * Find node with one of the given opcodes in the direct descendants
   * the current node.
   *
   * @param opcodes List of opcodes to be searched.
   * @return The matched node. Null if no node found.
   */
  public Xnode matchDirectDescendant(List<Xcode> opcodes) {
    List<Xnode> children = children();
    for(Xnode child : children) {
      if(opcodes.contains(child.opcode())) {
        return child;
      }
    }
    return null;
  }

  /**
   * Find node with the given path of opcodes from  the current node.
   *
   * @param opcodes Sequence of opcode to reach the node.
   * @return The matched node. Null if no node found.
   */
  public Xnode matchSeq(Xcode... opcodes) {
    Xnode tmp = this;
    for(Xcode opcode : opcodes) {
      tmp = tmp.matchDirectDescendant(opcode);
      if(tmp == null) {
        return null;
      }
    }
    return tmp;
  }

  /**
   * Match all nodes with the given opcode in the subtree.
   *
   * @param opcode Opcode of the nodes to be matched.
   * @return List of all nodes matched in the subtree.
   */
  public List<Xnode> matchAll(Xcode opcode) {
    List<Xnode> elements = new ArrayList<>();
    if(_baseElement == null) {
      return elements;
    }
    NodeList nodes = _baseElement.getElementsByTagName(opcode.code());
    for(int i = 0; i < nodes.getLength(); i++) {
      Node n = nodes.item(i);
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        elements.add(new Xnode((Element) n));
      }
    }
    return elements;
  }

  /**
   * Find an element either in the next siblings or in the ancestors.
   *
   * @param opcode Opcode of the node to be matched.
   * @param down   If True, search in the siblings. If false, search in the
   *               ancestors.
   * @return The matched node. Null if no node found.
   */
  private Xnode universalMatch(Xcode opcode, boolean down) {
    if(_baseElement == null) {
      return null;
    }

    Node nextNode = down ? _baseElement.getNextSibling() :
        _baseElement.getParentNode();

    while(nextNode != null) {
      if(nextNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) nextNode;
        if(element.getTagName().equals(opcode.code())) {
          return new Xnode(element);
        }
      }
      nextNode = down ? nextNode.getNextSibling() : nextNode.getParentNode();
    }
    return null;
  }

  /**
   * Get the depth of the node in the AST.
   *
   * @return A depth value greater or equal to 0.
   */
  public int depth() {
    if(_baseElement == null) {
      return Xnode.UNDEF_DEPTH;
    }
    return XnodeUtil.getDepth(_baseElement);
  }


  /**
   * Insert a node just after this node.
   *
   * @param node The node to be inserted after the current one.
   */
  public void insertAfter(Xnode node) {
    XnodeUtil.insertAfter(_baseElement, node.element());
  }

  /**
   * Insert a node just before this node.
   *
   * @param node The node to be inserted before the current one.
   */
  public void insertBefore(Xnode node) {
    if(_baseElement == null) {
      return;
    }
    _baseElement.getParentNode().insertBefore(node.element(), _baseElement);
  }

  /**
   * Find module definition node in which the current node is nested if any.
   *
   * @return A XmoduleDefinition node if found. Null otherwise.
   */
  public XmoduleDefinition findParentModule() {
    Xnode moduleDef = matchAncestor(Xcode.FMODULEDEFINITION);
    return (moduleDef != null) ?
        new XmoduleDefinition(moduleDef.element()) : null;
  }

  /**
   * Check whether a node is nested into another one.
   *
   * @param ancestor Node in which the current node is supposed to be nested.
   * @return True if the current node is nested in the ancestor node. False
   * otherwise.
   */
  public boolean isNestedIn(Xnode ancestor) {
    Node possibleAncestor = element().getParentNode();
    while(possibleAncestor != null) {
      if(possibleAncestor == ancestor.element()) {
        return true;
      }
      possibleAncestor = possibleAncestor.getParentNode();
    }
    return false;
  }

  @Override
  public boolean equals(Object obj) {
    return !(obj == null || !(obj instanceof Xnode))
        && element() == ((Xnode) obj).element();
  }
}
