/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xbody represents the body (8.7) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( statementModel* )
 * - statementModel TODO not useful at the moment
 *
 * @author clementval
 */

public class Xbody extends XbaseElement implements Xclonable<Xbody> {

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public Xbody(Element baseElement){
    super(baseElement);
  }

  /**
   * Insert XbaseElement at the beginning of the body children's list.
   * @param element Element to be inserted.
   */
  public void appendAsFirst(XbaseElement element){
    if(baseElement == null){
      throw new NullPointerException("body element is null");
    }

    Node firstElement = baseElement.getFirstChild();
    if(firstElement == null){
      baseElement.appendChild(element.getBaseElement());
    } else {
      baseElement.insertBefore(element.getBaseElement(), firstElement);
    }
  }


  @Override
  public Xbody cloneObject() {
    Element clone = (Element)cloneNode();
    return new Xbody(clone);
  }
}
