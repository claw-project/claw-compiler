package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The Xconstant represents the base of constants elements (7.1.1) element in
 * XcodeML intermediate representation. It includes (FintConstant,
 * FrealConstant, FcomplexConstant, FcharacterConstant, FlogicalConstant)
 *
 * Elements: contains value
 * Attributes:
 * - Optional: type (text), kind (text)
 */

public class Xconstant extends XbaseElement {
  private String _value = null;
  private String _type = null;
  private String _kind = null;

  public Xconstant(Element element){
    super(element);
    readElementInformation();
  }

  private void readElementInformation(){
    _value = baseElement.getTextContent();
    _type = XelementHelper.getAttributeValue(baseElement,
      XelementName.ATTR_TYPE);
    _kind = XelementHelper.getAttributeValue(baseElement,
      XelementName.ATTR_TYPE);
  }

  public boolean hasKind(){
    return _kind != null;
  }

  public void setKind(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_KIND, value);
      _kind = value;
    }
  }

  public boolean hasType(){
    return _type != null;
  }

  public void setType(String value){
    if(baseElement != null){
      baseElement.setAttribute(XelementName.ATTR_TYPE, value);
      _type = value;
    }
  }

  public String getValue(){
    return _value;
  }

  public void setValue(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _value = value;
    }
  }

  protected Xconstant create(XcodeProg xcodeml, String value, String type, String kind, XconstType constType){
    if(value == null){
      return null;
    }

    Xconstant constant;
    Element base;
    switch(constType){
      case INT:
        base = xcodeml.getDocument().
          createElement(XelementName.F_CHAR_CONST);
        constant = new XintConstant(base);
        break;
      case REAL:
        base = xcodeml.getDocument().
          createElement(XelementName.F_CHAR_CONST);
        constant = new XintConstant(base);
        break;
      case CHAR:
        base = xcodeml.getDocument().
          createElement(XelementName.F_CHAR_CONST);
        constant = new XintConstant(base);
        break;
      case LOGICAL:
        base = xcodeml.getDocument().
          createElement(XelementName.F_CHAR_CONST);
        constant = new XintConstant(base);
        break;
      default:
        return null;
    }

    constant.setValue(value);
    if(type != null){
      constant.setType(type);
    }

    if(kind != null){
      constant.setKind(kind);
    }

    return constant;
  }
}
