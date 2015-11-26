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
  protected XloopIterationRange _iterationRange;

  public XdoStatement(Element doStatementElement){
    super(doStatementElement);
    findRangeElements();
  }

  public void findRangeElements(){
    Element inductionVarElement = XelementHelper.findVar(baseElement);
    Element indexRangeElement = XelementHelper.findIndexRange(baseElement);

    _iterationRange =
      new XloopIterationRange(inductionVarElement, indexRangeElement);
  }

  public void setNewRange(XloopIterationRange range){
    Element body = getBodyElement();
    Node newVar = range.getInductionVar().clone();
    Node newRange = range.getIndexRange().clone();
    baseElement.insertBefore(newVar, body);
    baseElement.insertBefore(newRange, body);
    findRangeElements();
  }

  public void deleteRangeElements(){
    baseElement.removeChild(_iterationRange.getInductionVar().getBaseElement());
    baseElement.removeChild(_iterationRange.getIndexRange().getBaseElement());
  }

  protected void swapRangeElementsWith(XdoStatement otherLoop){
    otherLoop.setNewRange(_iterationRange);
    setNewRange(otherLoop.getIterationRange());
  }

  public XloopIterationRange getIterationRange(){
    return _iterationRange;
  }

  public Element getBodyElement(){
    return XelementHelper.getBody(baseElement);
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
}
