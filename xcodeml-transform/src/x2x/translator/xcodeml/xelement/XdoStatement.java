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
  private XloopIterationRange _iterationRange = null;
  private Xbody _body = null;

  public XdoStatement(Element doStatementElement){
    super(doStatementElement);
    findRangeElements();
    Element bodyElement = XelementHelper.getBody(baseElement);
    if(bodyElement != null){
      _body = new Xbody(bodyElement);
    }
  }

  public void findRangeElements(){
    Xvar inductionVar = XelementHelper.findVar(this);
    XindexRange indexRange = XelementHelper.findIndexRange(this);

    if(inductionVar != null && indexRange != null){
      _iterationRange =
        new XloopIterationRange(inductionVar, indexRange);
    }
  }

  /**
   * Create an empty arrayIndex element in the given program
   */
  public static XdoStatement createEmpty(XcodeProg xcodeml,
    XloopIterationRange range)
  {
    Element element = xcodeml.getDocument().
      createElement(XelementName.DO_STMT);

    if(range != null){
      element.appendChild(range.getInductionVar().clone());
      element.appendChild(range.getIndexRange().clone());
    }

    Element body = xcodeml.getDocument().createElement(XelementName.BODY);
    element.appendChild(body);

    return new XdoStatement(element);
  }

  public void setNewRange(XloopIterationRange range){
    Element body = _body.getBaseElement();
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

  public Xbody getBody(){
    return _body;
  }

  public void appendToBody(XdoStatement otherLoop){
    XelementHelper.appendBody(this.getBaseElement(),
      otherLoop.getBaseElement());
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
