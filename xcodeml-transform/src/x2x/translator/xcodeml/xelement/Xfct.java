package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class Xfct extends XbaseElement {
  private Xname _fctName = null;

  public Xfct(Element fctElement){
    super(fctElement);
    readElementInformation();
  }

  private void readElementInformation(){
    NodeList names = baseElement.getElementsByTagName(XelementName.NAME);
    Element nameElement = (Element) names.item(0);
    _fctName = new Xname(nameElement);
  }

  public void updateName(String value){
    _fctName.setName(value);
  }

  public void updateType(String value){
    _fctName.setType(value);
  }

  public void setName(String value){
    _fctName.setName(value);
  }

  public void setType(String value){
    _fctName.setType(value);
  }

  public String getFctName(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getValue();
  }

  public String getFctType(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getType();
  }
}
