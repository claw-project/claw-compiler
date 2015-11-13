package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

public class CLAWloop {
  protected Element _pragmaElement = null;
  protected Element _loopElement = null;

  protected Element _indexRangeElement = null;
  protected Element _inductionVarElement = null;

  protected CLAWloopIterationRange _iterationRange;

  public CLAWloop(Element pragma, Element loop){
    _pragmaElement = pragma;
    _loopElement = loop;
    findRangeElements();
  }

  protected void findRangeElements(){
    _inductionVarElement = CLAWelementHelper.findVar(_loopElement);
    _indexRangeElement = CLAWelementHelper.findIndexRange(_loopElement);

    _iterationRange =
      new CLAWloopIterationRange(_inductionVarElement, _indexRangeElement);
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

  protected void swapRangeElementsWith(CLAWloop otherLoop){
    otherLoop.setNewRange(_inductionVarElement, _indexRangeElement);
    setNewRange(otherLoop.getRangeVarElement(), otherLoop.getRangeElement());
  }

  public CLAWloopIterationRange getIterationRange(){
    return _iterationRange;
  }

  public Element getLoopElement(){
    return _loopElement;
  }

  public Element getPragmaElement(){
    return _pragmaElement;
  }

  public Element getBodyElement(){
    return CLAWelementHelper.getBody(_loopElement);
  }

  public String getOriginalFilename(){
    return CLAWelementHelper
      .getAttributeValue(_pragmaElement, XelementName.ATTR_FILE);
  }

  public String getPragmaLine(){
    return CLAWelementHelper
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

  public boolean hasSameRangeWith(CLAWloop other){
    return _iterationRange.isFullyIdentical(other.getIterationRange());
  }

  public Element getRangeElement(){
    return _indexRangeElement;
  }

  public Element getRangeVarElement(){
    return _inductionVarElement;
  }


}
