package x2x.translator.xcodeml;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;

public class CLAWelementHelper {

  public static String getAttributeValue(Element el, String attrName){
    NamedNodeMap attributes = el.getAttributes();
    for (int j = 0; j < attributes.getLength(); j++) {
      if(attributes.item(j).getNodeName().equals(attrName)){
        return attributes.item(j).getNodeValue();
      }
    }
    return null;
  }

  public static CLAWfctDef findFunctionDefinition(Document xcodemlDoc,
    CLAWfctCall fctCall)
  {
    if(xcodemlDoc == null || fctCall == null) {
      return null;
    }
    return findFunctionDefinition(xcodemlDoc, fctCall.getFctName(),
      fctCall.getFctType());
  }

  public static CLAWfctDef findFunctionDefinition(Document xcodemlDoc, String name,
    String type)
  {
    NodeList nList = xcodemlDoc.getElementsByTagName(XelementName.FCT_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        Element fctDefElement = (Element) fctDefNode;
        CLAWname fctDefName = findName(fctDefElement);
        if(name != null && fctDefName.isIdentical(name, type)){
          return new CLAWfctDef(fctDefElement);
        }
      }
    }
    return null;
  }

  public static CLAWfctDef findParentFctDef(Element child){
    Node parent = child.getParentNode();
    while(child.getParentNode() != null){
      if (parent.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) parent;
        if(element.getTagName().equals(XelementName.FCT_DEFINITION)){
          return new CLAWfctDef(element);
        }
      }
      parent = parent.getParentNode();
    }
    return null;
  }

  public static CLAWloop findLoop(CLAWfctDef fctDef){
    Element body = fctDef.getBody();
    Element loopElement = CLAWelementHelper.findLoopStament(body);
    if(loopElement == null){
      return null;
    }
    return new CLAWloop(null, loopElement);
  }

  public static Element findVar(Element parent){
    return findFirstElement(parent, XelementName.VAR);
  }

  public static Element findIndexRange(Element parent){
    return findFirstElement(parent, XelementName.INDEX_RANGE);
  }

  public static CLAWname findName(Element parent){
    Element element = findFirstElement(parent, XelementName.NAME);
    return (element != null) ? new CLAWname(element) : null;
  }

  public static Element getBody(Element parent){
    return findFirstElement(parent, XelementName.BODY);
  }

  public static Element findLoopStament(Element parent){
    return findFirstElement(parent, XelementName.DO_STMT);
  }

  public static Element findSymbols(Element parent){
    return findFirstElement(parent, XelementName.DO_STMT);
  }

  public static Element findFirstElement(Element parent, String elementName){
    NodeList elements = parent.getElementsByTagName(elementName);
    Element element = (Element) elements.item(0);
    return element;
  }

}
