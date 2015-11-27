package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XvarRef represents the varRef (7.4.6) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required: one of the followings
 *   - Var (Xvar)
 *   - FmemberRef TODO
 *   - FarrayRef (XarrayRef)
 *   - FcharacterRef TODO
 *   - FcoArrayRef TODO (not priority)
 * Attributes:
 * - Optional: type (text) TODO
 */

public class XvarRef extends XbaseElement {

  public XvarRef(Element varRefElement){
    super(varRefElement);
    readElementInformation();
  }

  private void readElementInformation(){
    // TODO
  }

  /**
   * Create an empty varRef element in the given program
   * param type attribute of the element. If null, no attribute is set
   */
  public static XvarRef createEmpty(XcodeProg xcodeml, String type){
    Element arrayRef = xcodeml.getDocument().
      createElement(XelementName.VAR_REF);
    if(type != null){
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    return new XvarRef(arrayRef);
  }

  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.clone();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    // TODO set the correct variable once they are there
  }
}
