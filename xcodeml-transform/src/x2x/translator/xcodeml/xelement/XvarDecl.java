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

  public XvarDecl(Element varDeclElement){
    super(varDeclElement);
    readElementInformation();
  }

  private void readElementInformation(){
    Element nameElement = XelementHelper.findFirstElement(baseElement,
      XelementName.NAME);
    _name = new Xname(nameElement);

    // TODO move to an object
    _valueElement = XelementHelper.findFirstElement(baseElement,
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

  public void append(XbaseElement element){
    append(element, false);
  }

  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.clone();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    if(element instanceof Xname){
      _name = (Xname)element; // TODO error if there is a name already
    }
  }

  /**
   * Create an empty varDecl element in the given program
   * @param nameValue  Value of the element required in a varDecl
   * @param nameType   Type of the name element requited in a varDecl
   */
  public static XvarDecl createEmpty(XcodeProg xcodeml, String nameValue,
    String nameType)
  {
    Xname name = Xname.createEmpty(xcodeml, nameValue, nameType);
    Element varDeclElement = xcodeml.getDocument().
      createElement(XelementName.VAR_DECL);
    XvarDecl varDecl = new XvarDecl(varDeclElement);
    varDecl.append(name);
    return varDecl;
  }

}
