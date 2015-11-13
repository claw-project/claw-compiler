package x2x.translator.xcodeml;

import org.w3c.dom.Element;
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

}
