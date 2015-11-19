package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

public class Xloop {
  protected Element _pragmaElement = null;
  protected Element _loopElement = null;

  protected Element _indexRangeElement = null;
  protected Element _inductionVarElement = null;

  protected XloopIterationRange _iterationRange;

  public Xloop(Element pragma, Element loop){
    _pragmaElement = pragma;
    _loopElement = loop;
    findRangeElements();
  }

  public void findRangeElements(){
    _inductionVarElement = XelementHelper.findVar(_loopElement);
    _indexRangeElement = XelementHelper.findIndexRange(_loopElement);

    _iterationRange =
      new XloopIterationRange(_inductionVarElement, _indexRangeElement);
  }

  public void setNewRange(Node var, Node range){
    Element body = getBodyElement();
    Node newVar = var.cloneNode(true);
    Node newRange = range.cloneNode(true);
    _loopElement.insertBefore(newVar, body);
    _loopElement.insertBefore(newRange, body);
    findRangeElements();
  }

  public void deleteRangeElements(){
    _loopElement.removeChild(_inductionVarElement);
    _loopElement.removeChild(_indexRangeElement);
  }

  protected void swapRangeElementsWith(Xloop otherLoop){
    otherLoop.setNewRange(_inductionVarElement, _indexRangeElement);
    setNewRange(otherLoop.getRangeVarElement(), otherLoop.getRangeElement());
  }

  public XloopIterationRange getIterationRange(){
    return _iterationRange;
  }

  public Element getLoopElement(){
    return _loopElement;
  }

  public Element getPragmaElement(){
    return _pragmaElement;
  }

  public Element getBodyElement(){
    return XelementHelper.getBody(_loopElement);
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

  public boolean hasSameRangeWith(Xloop other){
    return _iterationRange.isFullyIdentical(other.getIterationRange());
  }

  public Element getRangeElement(){
    return _indexRangeElement;
  }

  public Element getRangeVarElement(){
    return _inductionVarElement;
  }


}
