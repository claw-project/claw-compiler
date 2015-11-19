package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class Xfct {
  private Element _fctElement = null;
  private Xname _fctName = null;

  public Xfct(Element fctElement){
    _fctElement = fctElement;
    readElementInformation();
  }

  private void readElementInformation(){
    NodeList names = _fctElement.getElementsByTagName(XelementName.NAME);
    Element nameElement = (Element) names.item(0);
    _fctName = new Xname(nameElement);
  }

  public void updateName(String value){
    _fctName.setName(value);
  }

  protected Element getFctElement(){
    return _fctElement;
  }

  public String getFctName(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getName();
  }

  public String getFctType(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getType();
  }
}
