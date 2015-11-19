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

}
