package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;


/**
 * The XarrayRef represents the FarrayRef (7.4.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - varRef
 * - Optional:
 *   - arrayIndex TODO
 *   - indexRange TODO
 *   - FarrayConstructor TODO
 *   - FarrayRef TODO (XarrayRef)
 * Attributes:
 * - Optional: type (text)
 */

public class XarrayRef extends XbaseElement {

  private String _type = null;
  private XvarRef _varRef = null;
  private ArrayList<XbaseElement> _innerElement = null;

  public XarrayRef(Element arrayRefElement){
    super(arrayRefElement);
    _innerElement = new ArrayList<XbaseElement>();
    readElementInformation();
  }

  public ArrayList<XbaseElement> getInnerElements(){
    return _innerElement;
  }

  private void readElementInformation(){
    _type = XelementHelper.getAttributeValue(baseElement,
      XelementName.ATTR_TYPE);

    // Find Var element
    Element varElement = XelementHelper.findVarRef(baseElement);
    if(varElement != null){
      _varRef = new XvarRef(varElement);
    }

    // Read potential arrayIndex
    NodeList nodeList = baseElement.
      getElementsByTagName(XelementName.ARRAY_INDEX);

    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XarrayIndex arrayIndex = new XarrayIndex(el);
        _innerElement.add(arrayIndex);
      }
    }

    // TODO read indexRange

    // TODO read FarrayConstructor

    // TODO read FarrayRef

  }

  public XvarRef getVarRef(){
    return _varRef;
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

    if(element instanceof XvarRef){
      _varRef = (XvarRef)element;
    }
  }

  /**
   * Create an empty arrayRef element in the given program
   * param type attribute of the element. If null, no attribute is set
   */
  public static XarrayRef createEmpty(XcodeProg xcodeml, String type){
    Element arrayRef = xcodeml.getDocument().
      createElement(XelementName.F_ARRAY_REF);
    if(type != null){
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    return new XarrayRef(arrayRef);
  }
}
