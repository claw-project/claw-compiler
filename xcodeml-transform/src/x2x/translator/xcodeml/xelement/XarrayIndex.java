package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Map;

/**
 * The XarrayIndex represents the arrayIndex (8.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - exprModel TODO
 */

public class XarrayIndex extends XbaseElement {

  public XarrayIndex(Element arrayIndexElement){
    super(arrayIndexElement);
  }

  /**
   * Create an empty arrayIndex element in the given program
   */
  public static XarrayIndex createEmpty(XcodeProg xcodeml){
    Element arrayIndex = xcodeml.getDocument().
      createElement(XelementName.ARRAY_INDEX);
    return new XarrayIndex(arrayIndex);
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
  }

}
