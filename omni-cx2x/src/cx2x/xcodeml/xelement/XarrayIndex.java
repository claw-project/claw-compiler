package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Map;

/**
 * The XarrayIndex represents the arrayIndex (8.10) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - exprModel (XbaseElement)
 */

public class XarrayIndex extends XbaseElement {
  private XexprModel _exprModel;

  public XarrayIndex(Element arrayIndexElement){
    super(arrayIndexElement);
    readElementInformation();
  }

  private void readElementInformation(){
    // Find Var element if there is one
    // TODO move to XexprModel
    Xvar var = XelementHelper.findVar(this, false);
    if(var != null){
      _exprModel = new XexprModel(var);
    }
  }

  public XexprModel getExprModel(){
    return _exprModel;
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
