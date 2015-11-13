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

  public static Element findFunctionDefinition(Document xcodemlDoc,
    CLAWfctCall fctCall)
  {
    if(xcodemlDoc == null || fctCall == null) {
      return null;
    }
    return findFunctionDefinition(xcodemlDoc, fctCall.getFctName(),
      fctCall.getFctType());
  }

  public static Element findFunctionDefinition(Document xcodemlDoc, String name,
    String type)
  {
    NodeList nList = xcodemlDoc.getElementsByTagName(XelementName.FCT_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        Element fctDefElement = (Element) fctDefNode;
        CLAWname fctDefName = findName(fctDefElement);
        if(name != null && fctDefName.isIdentical(name, type)){
          return fctDefElement;
        }
      }
    }
    return null;
  }

  public static CLAWname findName(Element parent){
    NodeList names = parent.getElementsByTagName(XelementName.NAME);
    Element nameElement = (Element) names.item(0);
    if(nameElement == null){
      return null;
    }
    return new CLAWname(nameElement);
  }

  public static Element getBody(Element parent){
    NodeList elements = parent.getElementsByTagName(XelementName.BODY);
    Element element = (Element) elements.item(0);
    if(element == null){
      return null;
    }
    return element;
  }

  public static Element findLoopStament(Element parent){
    NodeList elements = parent.getElementsByTagName(XelementName.DO_STMT);
    Element element = (Element) elements.item(0);
    if(element == null){
      return null;
    }
    return element;
  }

}
