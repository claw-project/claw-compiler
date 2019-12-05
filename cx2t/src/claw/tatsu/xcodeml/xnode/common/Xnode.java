/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.Collections;
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
   * Delete this nodes with all its next siblings.
   */
  public void deleteWithSiblings() {
    List<Xnode> toDelete = new ArrayList<>();
    toDelete.add(this);
    Xnode sibling = nextSibling();
    while(sibling != null) {
      toDelete.add(sibling);
      sibling = sibling.nextSibling();
    }
    for(Xnode n : toDelete) {
      n.delete();
    }
  }

  /**
   * Get the element opcode.
   *
   * @return Opcode.
   */
  public Xcode opcode() {
    if(_baseElement == null) {
      return Xcode.NONE;
    }
    return Xcode.fromString(_baseElement.getTagName().toLowerCase());
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
  private boolean hasAttribute(String attrCode) {
    return _baseElement != null && _baseElement.hasAttribute(attrCode);
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
  private String getAttribute(String attrCode) {
    if(_baseElement != null && _baseElement.hasAttribute(attrCode)) {
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
  private boolean getBooleanAttribute(String attrCode) {
    return hasAttribute(attrCode) &&
        _baseElement.getAttribute(attrCode).equals(Xname.TRUE);
  }

  /**
   * Get raw node value.
   *
   * @return Raw value.
   */
  public String value() {
    return _baseElement == null ? "" :
        _baseElement.getTextContent().trim().toLowerCase();
  }

  /**
   * Set the element value.
   *
   * @param value The element value.
   * @return Current object to allow chaining.
   */
  public Xnode setValue(String value) {
    if(_baseElement != null) {
      _baseElement.setTextContent(value);
    }
    return this;
  }

  /**
   * Set the value of a boolean attribute.
   *
   * @param attrCode Attribute code.
   * @param value    Boolean value to set.
   * @return Current object to allow chaining.
   */
  public Xnode setBooleanAttribute(Xattr attrCode, boolean value) {
    return setBooleanAttribute(attrCode.toString(), value);
  }

  /**
   * Set the value of a boolean attribute.
   *
   * @param attrCode Attribute code.
   * @param value    Boolean value to set.
   * @return Current object to allow chaining.
   */
  private Xnode setBooleanAttribute(String attrCode, boolean value) {
    setAttribute(attrCode, value ? Xname.TRUE : Xname.FALSE);
    return this;
  }

  /**
   * Set attribute value.
   *
   * @param attrCode Attribute code.
   * @param value    Value of the attribute.
   * @return Current object to allow chaining.
   */
  public Xnode setAttribute(Xattr attrCode, String value) {
    return setAttribute(attrCode.toString(), value);
  }

  /**
   * Set attribute value.
   *
   * @param attrCode Attribute code.
   * @param value    Value of the attribute.
   * @return Current object to allow chaining.
   */
  private Xnode setAttribute(String attrCode, String value) {
    if(_baseElement != null && value != null) {
      _baseElement.setAttribute(attrCode, value);
    }
    return this;
  }

  /**
   * Remove an attribute from the node.
   *
   * @param attrCode Attribute code.
   */
  public void removeAttribute(Xattr attrCode) {
    if(_baseElement != null && _baseElement.hasAttribute(attrCode.toString())) {
      _baseElement.removeAttribute(attrCode.toString());
    }
  }

  /**
   * Check whether the current element is an element with body element.
   *
   * @return True if the element should have a body. False otherwise.
   */
  public boolean hasBody() {
    return opcode().hasBody();
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
    if(_baseElement == null) {
      return nodes;
    }
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
    return children.isEmpty() ? null : children.get(0);
  }

  /**
   * Get the last child node.
   *
   * @return Last child or null if no child exists.
   */
  public Xnode lastChild() {
    List<Xnode> children = this.children();
    return children.isEmpty() ? null : children.get(children.size() - 1);
  }

  /**
   * Delete the stored root element and all its children.
   */
  public void delete() {
    _isDeleted = true;
    if(_baseElement == null || _baseElement.getParentNode() == null) {
      return;
    }
    _baseElement.getParentNode().removeChild(_baseElement);
  }

  /**
   * Check whether the node has been deleted.
   *
   * @return True if the node has been deleted. False otherwise.
   */
  public boolean isDeleted() {
    return _isDeleted || _baseElement == null;
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
   * @return Current object to allow chaining.
   */
  public Xnode append(Xnode node, boolean clone) {
    if(node != null && _baseElement != null) {
      if(clone) {
        _baseElement.appendChild(node.cloneRawNode());
      } else {
        _baseElement.appendChild(node.element());
      }
    }
    return this;
  }

  /**
   * Append an element ot the children of this element. Node is not cloned.
   *
   * @param node The element to append.
   * @return Current object to allow chaining.
   */
  public Xnode append(Xnode node) {
    append(node, false);
    return this;
  }

  /**
   * Insert as first child.
   *
   * @param node  Node to be inserted.
   * @param clone Clone or not the element before insertion.
   */
  public void insert(Xnode node, boolean clone) {
    if(node != null && _baseElement != null) {
      NodeList children = _baseElement.getChildNodes();
      Node toInsert = clone ? node.cloneRawNode() : node.element();
      if(children.getLength() == 0) {
        append(node, clone);
      } else {
        _baseElement.insertBefore(toInsert, children.item(0));
      }
    }
  }

  /**
   * Insert as first child.
   *
   * @param node Node to be inserted.
   * @return Current object to allow chaining.
   */
  public Xnode insert(Xnode node) {
    insert(node, false);
    return this;
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
    if(hasAttribute(Xattr.LINENO)) {
      return Integer.parseInt(getAttribute(Xattr.LINENO));
    }
    return getClosestLineNo();
  }

  /**
   * Get the file attribute value. This value represents the original filename
   * from the XcodeML unit. This attribute is not defined for every elements.
   *
   * @return File path. Empty string if the file attribute is not defined.
   */
  public String filename() {
    return hasAttribute(Xattr.FILE) ? getAttribute(Xattr.FILE) : "";
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
    if(_baseElement == null) {
      return null;
    }
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
   * Get ancestor node if any.
   *
   * @return Ancestor node if any. Null otherwise.
   */
  public Xnode ancestor() {
    if(_baseElement == null || _baseElement.getParentNode() == null) {
      return null;
    }

    // Reach document root stop here
    if(_baseElement.getParentNode().getNodeType() == Node.DOCUMENT_NODE) {
      return null;
    }

    return new Xnode((Element) _baseElement.getParentNode());
  }

  /**
   * Get previous sibling node.
   *
   * @return Previous sibling node.
   */
  public Xnode prevSibling() {
    if(_baseElement == null) {
      return null;
    }
    Node n = _baseElement.getPreviousSibling();
    while(n != null) {
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        return new Xnode((Element) n);
      }
      n = n.getPreviousSibling();
    }
    return null;
  }

  /**
   * Check whether the end node is a direct sibling of this node. If other
   * nodes are between the two nodes and their opcode is not listed in the
   * skippedNodes list, the nodes are not direct siblings.
   *
   * @param end          Node to be check to be a direct sibling.
   * @param skippedNodes List of opcode that are allowed between the two nodes.
   * @return True if the nodes are direct siblings.
   */
  public boolean isDirectSibling(Xnode end, List<Xcode> skippedNodes) {
    if(end == null) {
      return false;
    }

    Xnode nextSibling = nextSibling();
    while(nextSibling != null) {
      if(nextSibling.equals(end)) {
        return true;
      }
      if(skippedNodes.contains(nextSibling.opcode())) {
        nextSibling = nextSibling.nextSibling();
      } else {
        return false;
      }
    }
    return false;
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
   * Find nodes with the given opcode in the ancestors of the current node.
   *
   * @param opcode Opcode of the node to be matched.
   * @return List of matched node. Empty list if nothing matched.
   */
  public List<Xnode> matchAllAncestor(Xcode opcode) {
    return matchAllAncestor(opcode, null);
  }

  /**
   * Find nodes with the given opcode in the ancestors of the current node.
   *
   * @param opcode   Opcode of the node to be matched.
   * @param stopCode Stop searching if given opcode is reached. If null, search
   *                 until root node.
   * @return List of matched node. Empty list if nothing matched.
   */
  public List<Xnode> matchAllAncestor(Xcode opcode, Xcode stopCode) {
    List<Xnode> statements = new ArrayList<>();
    Xnode crt = this;
    while(crt != null && crt.ancestor() != null) {
      if(crt.ancestor().opcode() == opcode) {
        statements.add(crt.ancestor());
      }
      // Stop searching when FfunctionDefinition is reached
      if(stopCode != null && crt.ancestor().opcode() == stopCode) {
        return statements;
      }
      crt = crt.ancestor();
    }
    return statements;
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
    return matchDirectDescendant(Collections.singletonList(opcode));
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
    List<Xnode> nodes = new ArrayList<>();
    if(_baseElement == null) {
      return nodes;
    }
    NodeList rawNodes = _baseElement.getElementsByTagName(opcode.code());
    for(int i = 0; i < rawNodes.getLength(); i++) {
      Node n = rawNodes.item(i);
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        nodes.add(new Xnode((Element) n));
      }
    }
    return nodes;
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
    if(isDeleted()) {
      return Xnode.UNDEF_DEPTH;
    }

    Node parent = _baseElement.getParentNode();
    int depth = 0;
    while(parent != null && parent.getNodeType() == Node.ELEMENT_NODE) {
      ++depth;
      parent = parent.getParentNode();
    }
    return depth;
  }

  /**
   * Insert a node just after this node.
   *
   * @param node The node to be inserted after the current one.
   */
  public void insertAfter(Xnode node) {
    if(_baseElement != null && node != null) {
      Node parent = _baseElement.getParentNode();
      if(parent != null) {
        parent.insertBefore(node.element(), _baseElement.getNextSibling());
      }
    }
  }

  /**
   * Compare the inner value of the first child of two nodes.
   *
   * @param other Node to compare with
   * @return True if the value if first childs are identical. False otherwise.
   */
  public boolean compareFirstChildValues(Xnode other) {
    if(other == null) {
      return false;
    }

    Xnode c1 = child(0);
    Xnode c2 = other.child(0);
    return c1 != null && c2 != null && c1.compareValues(c2);
  }

  /**
   * Compare the inner values of two optional nodes.
   *
   * @param other Node to compare with
   * @return True if the values are identical or elements are null. False
   * otherwise.
   */
  public boolean compareOptionalValues(Xnode other) {
    return other == null || value().equalsIgnoreCase(other.value());
  }

  /**
   * Compare the inner values of two nodes.
   *
   * @param other Node to compare with
   * @return True if the values are identical. False otherwise.
   */
  public boolean compareValues(Xnode other) {
    return other != null && value().equals(other.value());
  }

  /**
   * Insert a node just before this node.
   *
   * @param node The node to be inserted before the current one.
   */
  public void insertBefore(Xnode node) {
    if(_baseElement != null && node != null) {
      Node parent = _baseElement.getParentNode();
      if(parent != null) {
        parent.insertBefore(node.element(), _baseElement);
      }
    }
  }

  /**
   * Find module definition node in which the current node is nested if any.
   *
   * @return A FmoduleDefinition node if found. Null otherwise.
   */
  public FmoduleDefinition findParentModule() {
    Xnode moduleDef = matchAncestor(Xcode.F_MODULE_DEFINITION);
    return (moduleDef != null) ? new FmoduleDefinition(moduleDef) : null;
  }

  /**
   * Check if the ancestor if of the given opcode.
   *
   * @param opcode Opcode to check for
   * @return True if the ancestor if of the given opcode. False otherwise.
   */
  public boolean ancestorIs(Xcode opcode) {
    return ancestor() != null && ancestor().is(opcode);
  }

  /**
   * Check is the node is of the given opcode.
   *
   * @param opcode Opcode to check for.
   * @return True if the node is of the given opcode.
   */
  public boolean is(Xcode opcode) {
    return opcode() == opcode;
  }

  /**
   * Check is the current node is one of the constant node.
   *
   * @return True if the node is a constant node. False otherwise.
   */
  public boolean isConstant() {
    return opcode() == Xcode.F_REAL_CONSTANT
        || opcode() == Xcode.F_INT_CONSTANT
        || opcode() == Xcode.F_COMPLEX_CONSTANT
        || opcode() == Xcode.F_CHARACTER_CONSTANT
        || opcode() == Xcode.F_LOGICAL_CONSTANT;
  }

  /**
   * Check whether the given node is of the given opcode.
   *
   * @param node   Node to be checked.
   * @param opcode Opcode to be matched.
   * @return True if the node is not null and match the given opcode. False
   * otherwise.
   */
  public static boolean isOfCode(Xnode node, Xcode opcode) {
    return node != null && node.is(opcode);
  }

  /**
   * Check whether a node is nested into another one.
   *
   * @param ancestor Node in which the current node is supposed to be nested.
   * @return True if the current node is nested in the ancestor node. False
   * otherwise.
   */
  public boolean isNestedIn(Xnode ancestor) {
    if(ancestor == null || element() == null) {
      return false;
    }
    Node possibleAncestor = element().getParentNode();
    while(possibleAncestor != null) {
      if(possibleAncestor == ancestor.element()) {
        return true;
      }
      possibleAncestor = possibleAncestor.getParentNode();
    }
    return false;
  }

  /**
   * Return type hash associated with this node if any.
   *
   * @return Type hash. Empty string if there is no type hash associated.
   */
  public String getType() {
    if(_baseElement == null) {
      return "";
    }
    switch(opcode()) {
      case F_ARRAY_REF:
        String type = getAttribute(Xattr.TYPE);
        if(FortranType.isBuiltInType(type)) {
          Xnode child = firstChild();
          return (child != null) ? child.getAttribute(Xattr.TYPE) : "";
        }
        return type;
      case NAMED_VALUE:
        Xnode child = firstChild();
        return (child != null) ? child.getAttribute(Xattr.TYPE) : "";
      case F_FUNCTION_DEFINITION:
      case FUNCTION_CALL:
      case VAR_DECL:
        // functionCall has a type attribute but it's the return type
        Xnode name = matchDirectDescendant(Xcode.NAME);
        return (name != null) ? name.getAttribute(Xattr.TYPE) : "";
      default:
        return getAttribute(Xattr.TYPE);
    }
  }

  /**
   * Set type of the node.
   *
   * @param type FbasicType to be associated with this node.
   */
  public Xnode setType(FbasicType type) {
    return setAttribute(Xattr.TYPE, type.getType());
  }

  /**
   * Set the type of the current node.
   *
   * @param value Type value.
   */
  public Xnode setType(String value) {
    if(value != null && !value.isEmpty()) {
      setAttribute(Xattr.TYPE, value);
    }
    return this;
  }

  /**
   * Construct string representation of the node. Only for variable or constant.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  public String constructRepresentation(boolean withNamedValue,
                                        boolean nameOnly)
  {
    switch(opcode()) {
      case F_INT_CONSTANT:
      case F_PRAGMA_STATEMENT:
      case VAR:
        return value();
      case ARRAY_INDEX:
      case LOWER_BOUND:
      case UPPER_BOUND:
      case VAR_REF:
        return constructSimpleRepresentation(withNamedValue, nameOnly);
      case INDEX_RANGE:
        return nameOnly
            ? "" : constructIndexRangeRepresentation(withNamedValue, nameOnly);
      case F_ARRAY_REF:
        return constructArrayRefRepresentation(withNamedValue, nameOnly);
      case F_MEMBER_REF:
        return constructMemberRefRepresentation(withNamedValue, nameOnly);
      case NAMED_VALUE:
        return constructNamedValueRepresentation(withNamedValue, nameOnly);
      case MINUS_EXPR:
      case PLUS_EXPR:
      case MUL_EXPR:
      case DIV_EXPR:
      case F_CONCAT_EXPR:
      case F_POWER_EXPR:
      case LOG_AND_EXPR:
      case LOG_EQ_EXPR:
      case LOG_EQV_EXPR:
      case LOG_GE_EXPR:
      case LOG_GT_EXPR:
      case LOG_LE_EXPR:
      case LOG_LT_EXPR:
      case LOG_NEQ_EXPR:
      case LOG_NEWV_EXPR:
      case LOG_OR_EXPR:
        return constructBinaryExprRepresentation(withNamedValue, nameOnly);
      default:
        return "";
    }
  }

  /**
   * Construct string representation of the binary expression node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Empty if node is null.
   */
  private String constructBinaryExprRepresentation(boolean withNamedValue,
                                                   boolean nameOnly)
  {
    Xnode child0 = child(0);
    Xnode child1 = child(1);
    return ((child0 != null) ?
        child0.constructRepresentation(withNamedValue, nameOnly) : "") +
        getBinaryOpRepresentation()
        + ((child1 != null) ?
        child1.constructRepresentation(withNamedValue, nameOnly) : "");
  }

  /**
   * Get string representation of binary operator.
   *
   * @return Binary operator.
   */
  private String getBinaryOpRepresentation() {
    switch(opcode()) {
      case MINUS_EXPR:
        return "-";
      case PLUS_EXPR:
        return "+";
      case MUL_EXPR:
        return "*";
      case DIV_EXPR:
        return "/";
      case F_CONCAT_EXPR:
        return "//";
      case F_POWER_EXPR:
        return "**";
      case LOG_AND_EXPR:
        return ".and.";
      case LOG_EQ_EXPR:
        return ".eq.";
      case LOG_EQV_EXPR:
        return ".eqv.";
      case LOG_GE_EXPR:
        return ".ge.";
      case LOG_GT_EXPR:
        return ".gt.";
      case LOG_LE_EXPR:
        return ".le.";
      case LOG_LT_EXPR:
        return ".lt";
      case LOG_NEQ_EXPR:
        return ".neq";
      case LOG_NEWV_EXPR:
        return ".newv";
      case LOG_OR_EXPR:
        return ".or.";
      default:
        return "";
    }
  }

  /**
   * Construct string representation of the IndexRange node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  private String constructIndexRangeRepresentation(boolean withNamedValue,
                                                   boolean nameOnly)
  {
    if(getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)) {
      return ":";
    }
    Xnode child0 = child(0);
    Xnode child1 = child(1);
    return ((child0 != null) ?
        child0.constructRepresentation(withNamedValue, nameOnly) : "") + ":" +
        ((child1 != null) ?
            child1.constructRepresentation(withNamedValue, nameOnly) : "");
  }

  /**
   * Construct string representation of the simple node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  private String constructSimpleRepresentation(boolean withNamedValue,
                                               boolean nameOnly)
  {
    Xnode n = firstChild();
    return (n != null)
        ? n.constructRepresentation(withNamedValue, nameOnly) : "";
  }

  /**
   * Construct string representation of the ArrayRef node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  private String constructArrayRefRepresentation(boolean withNamedValue,
                                                 boolean nameOnly)
  {
    List<Xnode> childs = children();
    if(childs.size() == 1) {
      return childs.get(0).constructRepresentation(withNamedValue, nameOnly);
    } else {
      StringBuilder str = new StringBuilder();
      str.append(childs.get(0).constructRepresentation(withNamedValue,
          nameOnly));
      if(nameOnly) {
        return str.toString();
      }
      str.append("(");
      for(int i = 1; i < childs.size(); ++i) {
        str.append(childs.get(i).constructRepresentation(withNamedValue,
            nameOnly));
        if(i != childs.size() - 1) {
          str.append(",");
        }
      }
      str.append(")");
      return str.toString();
    }
  }

  /**
   * Construct string representation of the MemberRef node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  private String constructMemberRefRepresentation(boolean withNamedValue,
                                                  boolean nameOnly)
  {
    Xnode n = firstChild();
    return ((n != null) ?
        n.constructRepresentation(withNamedValue, nameOnly) + "%" +
            getAttribute(Xattr.MEMBER) : "");
  }

  /**
   * Construct string representation of the NamedValue node.
   *
   * @param withNamedValue If true, keeps the named value, otherwise, just
   *                       constructs the argument.
   * @param nameOnly       If true, index part of array is not constructed.
   * @return String representation. Null if node is null.
   */
  private String constructNamedValueRepresentation(boolean withNamedValue,
                                                   boolean nameOnly)
  {
    Xnode n = firstChild();
    if(withNamedValue) {
      return ((n != null) ? getAttribute(Xattr.NAME) + "=" +
          n.constructRepresentation(true, nameOnly) : "");
    }
    return (n != null) ? n.constructRepresentation(false, nameOnly) : "";
  }

  /**
   * Copy the enhanced information from the current node to a target node.
   * Enhanced information include line number and original file name.
   *
   * @param target Target node to copy information to.
   */
  public void copyEnhancedInfo(Xnode target) {
    target.setLine(lineNo());
    target.setFilename(filename());
  }

  /**
   * Check if the given node is direct children of the same parent node.
   *
   * @param n Node to be compared to.
   * @return True if the given node is direct children of the same parent. False
   * otherwise.
   */
  public boolean hasSameParentBlock(Xnode n) {
    return !(n == null || element() == null || n.element() == null)
        && element().getParentNode() == n.element().getParentNode();
  }

  /**
   * Find function definition in the ancestor of the current node.
   *
   * @return The function definition found. Null if nothing found.
   */
  public FfunctionDefinition findParentFunction() {
    Xnode fctDef = matchAncestor(Xcode.F_FUNCTION_DEFINITION);
    if(fctDef == null) {
      return null;
    }
    return new FfunctionDefinition(fctDef);
  }

  /**
   * Copy the attribute from the current node to the given node.
   *
   * @param to   Xnode to copy to.
   * @param attr Attribute code to be copied.
   */
  public void copyAttribute(Xnode to, Xattr attr) {
    if(hasAttribute(attr)) {
      to.setAttribute(attr, getAttribute(attr));
    }
  }

  /**
   * Sync given attribute between two node. Attribute present in this node
   * is created in "to" if not present yet. Attribute not present in this node
   * is removed from "to" if present.
   *
   * @param to        Node to which attribute is synced.
   * @param attribute Attribute to be synced.
   */
  public void syncBooleanAttribute(Xnode to, Xattr attribute)
  {
    if(getBooleanAttribute(attribute) && !to.getBooleanAttribute(attribute)) {
      to.setBooleanAttribute(attribute, true);
    } else if(!getBooleanAttribute(attribute)
        && to.getBooleanAttribute(attribute))
    {
      to.removeAttribute(attribute);
    }
  }

  /**
   * Return a brief description of the Xnode.
   *
   * @return String description of the Xnode as "Opcode (children: n)".
   */
  @Override
  public String toString() {
    return String.format("%s (children: %d) - %d", opcode().code(),
        children().size(), lineNo());
  }

  @Override
  public int hashCode() {
    return _baseElement != null ? _baseElement.hashCode() : 0;
  }

  @Override
  public boolean equals(Object o) {
    return o instanceof Xnode && element() == ((Xnode) o).element();
  }

  /**
   * Check whether current node is not part of an arrayIndex node.
   *
   * @return True if the node is not a direct child of an arrayIndex node.
   * False otherwise.
   */
  public boolean isNotArrayIndex() {
    return !Xnode.isOfCode(ancestor(), Xcode.ARRAY_INDEX);
  }

  /**
   * Check whether current node is not an array reference node.
   *
   * @return True if the node is not an array reference node. False otherwise.
   */
  public boolean isNotArrayRef() {
    return !ancestor().ancestorIs(Xcode.F_ARRAY_REF)
        && !ancestorIs(Xcode.F_ARRAY_REF);
  }

  /**
   * Find meaningful line number for error reporting.
   *
   * @return First line number found in ancestors.
   */
  private int getClosestLineNo() {
    Xnode crtAncestor = ancestor();
    while(crtAncestor != null) {
      if(crtAncestor.hasAttribute(Xattr.LINENO)) {
        return crtAncestor.lineNo();
      }
      crtAncestor = crtAncestor.ancestor();
    }
    return 0; // Default if not found
  }
}
