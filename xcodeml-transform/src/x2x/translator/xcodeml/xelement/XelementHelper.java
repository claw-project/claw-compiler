package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;

public class XelementHelper {

  public static String getAttributeValue(Element el, String attrName){
    NamedNodeMap attributes = el.getAttributes();
    for (int j = 0; j < attributes.getLength(); j++) {
      if(attributes.item(j).getNodeName().equals(attrName)){
        return attributes.item(j).getNodeValue();
      }
    }
    return null;
  }

  public static XfctDef findFunctionDefinition(Document xcodemlDoc,
    XfctCall fctCall)
  {
    if(xcodemlDoc == null || fctCall == null) {
      return null;
    }
    return findFunctionDefinition(xcodemlDoc, fctCall.getFctName(),
      fctCall.getFctType());
  }

  public static XfctDef findFunctionDefinition(Document xcodemlDoc, String name,
    String type)
  {
    NodeList nList = xcodemlDoc.getElementsByTagName(XelementName.FCT_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        Element fctDefElement = (Element) fctDefNode;
        Xname fctDefName = findName(fctDefElement);
        if(name != null && fctDefName.isIdentical(name, type)){
          return new XfctDef(fctDefElement);
        }
      }
    }
    return null;
  }

  public static XfctDef findParentFctDef(Element child){
    Node parent = child.getParentNode();
    while(child.getParentNode() != null){
      if (parent.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) parent;
        if(element.getTagName().equals(XelementName.FCT_DEFINITION)){
          return new XfctDef(element);
        }
      }
      parent = parent.getParentNode();
    }
    return null;
  }

  public static Xloop findLoop(XfctDef fctDef){
    Element body = fctDef.getBody();
    Element loopElement = XelementHelper.findLoopStament(body);
    if(loopElement == null){
      return null;
    }
    return new Xloop(null, loopElement);
  }

  public static Element findVar(Element parent){
    return findFirstElement(parent, XelementName.VAR);
  }

  public static Element findIndexRange(Element parent){
    return findFirstElement(parent, XelementName.INDEX_RANGE);
  }

  public static Xname findName(Element parent){
    Element element = findFirstElement(parent, XelementName.NAME);
    return (element != null) ? new Xname(element) : null;
  }

  public static Element getBody(Element parent){
    return findFirstElement(parent, XelementName.BODY);
  }

  public static Element findLoopStament(Element parent){
    return findFirstElement(parent, XelementName.DO_STMT);
  }

  public static Element findSymbols(Element parent){
    return findFirstElement(parent, XelementName.SYMBOLS);
  }

  public static Element findDeclarations(Element parent){
    return findFirstElement(parent, XelementName.DECLARATIONS);
  }

  public static Element findTypeTable(Document doc){
    NodeList elements = doc.getElementsByTagName(XelementName.TYPE_TABLE);
    Element element = (Element) elements.item(0);
    return element;
  }

  public static int findNumberOfRange(Element parent){
    NodeList elements = parent.getElementsByTagName(XelementName.INDEX_RANGE);
    return elements.getLength();
  }

  public static NodeList findIndexRanges(Element parent){
    return parent.getElementsByTagName(XelementName.INDEX_RANGE);
  }

  public static Element findLen(Element parent){
    NodeList elements = parent.getElementsByTagName(XelementName.LENGTH);
    if(elements.getLength() == 0){
      return null;
    }
    return (Element) elements.item(0);
  }

  public static Element findFirstElement(Element parent, String elementName){
    NodeList elements = parent.getElementsByTagName(elementName);
    Element element = (Element) elements.item(0);
    return element;
  }

  public static void insertAfter(Node refNode, Node newNode){
    refNode.getParentNode().insertBefore(newNode, refNode.getNextSibling());
  }


  public static Element findNextLoop(Node from){
    return findNextElementOfType(from, XelementName.DO_STMT);
  }

  public static Element findNextExprStatement(Node from){
    return findNextElementOfType(from, XelementName.EXPR_STMT);
  }

  private static Element findNextElementOfType(Node from, String tag){
    Node nextNode = from.getNextSibling();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(tag)){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  public static NodeList getPragmas(Document root){
    return root.getElementsByTagName(XelementName.PRAGMA_STMT);
  }

  /**
   * Removes text nodes that only contains whitespace. The conditions for
   * removing text nodes, besides only containing whitespace, are: If the
   * parent node has at least one child of any of the following types, all
   * whitespace-only text-node children will be removed: - ELEMENT child -
   * CDATA child - COMMENT child
   *
   * The purpose of this is to make the format() method (that use a
   * Transformer for formatting) more consistent regarding indenting and line
   * breaks.
   */
  public static void cleanEmptyTextNodes(Node parentNode) {
    boolean removeEmptyTextNodes = false;
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      removeEmptyTextNodes |= checkNodeTypes(childNode);
      childNode = childNode.getNextSibling();
    }

    if (removeEmptyTextNodes) {
      removeEmptyTextNodes(parentNode);
    }
  }

  private static void removeEmptyTextNodes(Node parentNode) {
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      // grab the "nextSibling" before the child node is removed
      Node nextChild = childNode.getNextSibling();

      short nodeType = childNode.getNodeType();
      if (nodeType == Node.TEXT_NODE) {
        boolean containsOnlyWhitespace = childNode.getNodeValue()
          .trim().isEmpty();
        if (containsOnlyWhitespace) {
          parentNode.removeChild(childNode);
        }
      }
      childNode = nextChild;
    }
  }

  private static boolean checkNodeTypes(Node childNode) {
    short nodeType = childNode.getNodeType();

    if (nodeType == Node.ELEMENT_NODE) {
      cleanEmptyTextNodes(childNode); // recurse into subtree
    }

    if (nodeType == Node.ELEMENT_NODE
        || nodeType == Node.CDATA_SECTION_NODE
        || nodeType == Node.COMMENT_NODE) {
      return true;
    } else {
      return false;
    }
  }

}
