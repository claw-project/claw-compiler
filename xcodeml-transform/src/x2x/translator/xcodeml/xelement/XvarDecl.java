package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XvarDecl represents the varDecl (5.4) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - name (with attribute "type (text)") // TODO move to Xname
 * - Optional:
 *   - value (text)
 */

public class XvarDecl {
  private String _type = null;
  private String _name = null;
  private boolean _hasValue = false;

  private Element _varDeclElement;

  // TODO keep as element for the moment but it should be read as well
  private Element _valueElement;

  public XvarDecl(Element varDeclElement){
    _varDeclElement = varDeclElement;
    readElementInformation();
  }

  public Node clone(){
    return _varDeclElement.cloneNode(true);
  }

  private void readElementInformation(){
    Element nameElement = XelementHelper.findFirstElement(_varDeclElement,
      XelementName.NAME);
    _name = nameElement.getTextContent();
    _type = XelementHelper.getAttributeValue(nameElement,
      XelementName.ATTR_TYPE);
    _valueElement = XelementHelper.findFirstElement(_varDeclElement,
        XelementName.VALUE);
    if(_valueElement != null){
      _hasValue = true;
    }
  }

  public boolean hasValue(){
    return _hasValue;
  }

  public String getName(){
    return _name;
  }

}
