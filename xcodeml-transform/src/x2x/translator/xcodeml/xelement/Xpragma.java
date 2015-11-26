package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xpragma represents the FpragmaStatement (6.25) element in XcodeML
 * intermediate representation.
 *
 * Elements: contains value of the pragma line.
 */

public class Xpragma extends XbaseElement {
  private String _value = null;
  private String _line = null;
  private String _filename = null;

  public Xpragma(Element pragmaElement){
    super(pragmaElement);
    if(pragmaElement != null){
      _value = getData();
      _line = XelementHelper.getAttributeValue(baseElement,
        XelementName.ATTR_LINENO);
      _filename = XelementHelper.getAttributeValue(baseElement,
        XelementName.ATTR_FILE);
    }
  }

  public String getFilename(){
    return _filename;
  }

  public String getLine(){
    return _line;
  }

  public Xpragma cloneObject(){
    Node clone = clone();
    return new Xpragma((Element)clone);
  }

  public void setData(String value){
    if(baseElement != null){
      baseElement.setTextContent(value);
      _value = value;
    }
  }
}
