package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XvarDecl represents the varDecl (5.4) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required:
 *   - name (Xname)
 * - Optional:
 *   - value (text) TODO have an object for that
 */

public class XvarDecl extends Xdecl {
  private Xname _name = null;
  private boolean _hasValue = false;

  private Element _valueElement = null; // TODO to be removed

  public XvarDecl(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  private void readElementInformation(){
    Element nameElement = XelementHelper.findFirstElement(_element,
      XelementName.NAME);
    _name = new Xname(nameElement);

    // TODO move to an object
    _valueElement = XelementHelper.findFirstElement(_element,
        XelementName.VALUE);
    if(_valueElement != null){
      _hasValue = true;
    }
  }

  public boolean hasValue(){
    return _hasValue;
  }

  public Xname getName(){
    return _name;
  }

}
