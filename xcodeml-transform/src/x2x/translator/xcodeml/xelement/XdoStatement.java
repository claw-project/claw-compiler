package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;


/**
 * The XdoStatement represents the FdoStatement (6.5) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - Var (Xvar)
 *   - indexRange (XindexRange)
 *   - body // TODO check what is the meaning of this value
 * Attributes:
 * - Optional: construct_name TODO
 */
public class XdoStatement extends XbaseElement {
  protected Element _pragmaElement = null;

  protected Element _indexRangeElement = null;
  protected Element _inductionVarElement = null;

  protected XloopIterationRange _iterationRange;

  public XdoStatement(Element pragma, Element doStatementElement){
    super(doStatementElement);
    _pragmaElement = pragma;
    findRangeElements();
  }

  public void findRangeElements(){
    _inductionVarElement = XelementHelper.findVar(baseElement);
    _indexRangeElement = XelementHelper.findIndexRange(baseElement);

    _iterationRange =
      new XloopIterationRange(_inductionVarElement, _indexRangeElement);
  }

  public void setNewRange(Node var, Node range){
    Element body = getBodyElement();
    Node newVar = var.cloneNode(true);
    Node newRange = range.cloneNode(true);
    baseElement.insertBefore(newVar, body);
    baseElement.insertBefore(newRange, body);
    findRangeElements();
  }

  public void deleteRangeElements(){
    baseElement.removeChild(_inductionVarElement);
    baseElement.removeChild(_indexRangeElement);
  }

  protected void swapRangeElementsWith(XdoStatement otherLoop){
    otherLoop.setNewRange(_inductionVarElement, _indexRangeElement);
    setNewRange(otherLoop.getRangeVarElement(), otherLoop.getRangeElement());
  }

  public XloopIterationRange getIterationRange(){
    return _iterationRange;
  }

  public Element getLoopElement(){
    return baseElement;
  }

  public Element getPragmaElement(){
    return _pragmaElement;
  }

  public Element getBodyElement(){
    return XelementHelper.getBody(baseElement);
  }

  public String getOriginalFilename(){
    return XelementHelper
      .getAttributeValue(_pragmaElement, XelementName.ATTR_FILE);
  }

  public String getPragmaLine(){
    return XelementHelper
      .getAttributeValue(_pragmaElement, XelementName.ATTR_LINENO);
  }

  public String getIterationVariableValue(){
    return _iterationRange.getInductionVar().getValue();
  }

  public String getLowerBoundValue(){
    return _iterationRange.getIndexRange().getLowerBound().getValue();
  }

  public String getUpperBoundValue(){
    return _iterationRange.getIndexRange().getUpperBound().getValue();
  }

  public String getStepValue(){
    return _iterationRange.getIndexRange().getStep().getValue();
  }

  public String getFormattedRange(){
    return _iterationRange.toString();
  }

  public boolean hasSameRangeWith(XdoStatement other){
    return _iterationRange.isFullyIdentical(other.getIterationRange());
  }

  public Element getRangeElement(){
    return _indexRangeElement;
  }

  public Element getRangeVarElement(){
    return _inductionVarElement;
  }


}
