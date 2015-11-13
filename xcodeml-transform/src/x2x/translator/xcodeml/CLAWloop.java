package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;

public class CLAWloop {
  protected Element _pragmaElement = null;
  protected Element _loopElement = null;

  protected Element _rangeElement = null;
  protected Element _rangeVarElement = null;

  protected String _iterationVar;
  protected String _lowerBoundValue;
  protected String _upperBoundValue;
  protected String _stepValue;




  public CLAWloop(Element pragma, Element loop){
    _pragmaElement = pragma;
    _loopElement = loop;

    findRangeElements();
  }

  protected void findRangeElements(){
    _rangeVarElement = CLAWelementHelper.findVar(_loopElement);
    _rangeElement = CLAWelementHelper.findIndexRange(_loopElement);

    _iterationVar = _rangeVarElement.getTextContent();
    _lowerBoundValue = getRangeValue("lowerBound");
    _upperBoundValue = getRangeValue("upperBound");
    _stepValue = getRangeValue("step");
  }

  private String getRangeValue(String tag){
    NodeList rangeElements = _loopElement.getElementsByTagName(tag);
    Element rangeElement = (Element) rangeElements.item(0);
    NodeList constants = rangeElement.getElementsByTagName("FintConstant"); // TODO string constant
    Element constant = (Element) constants.item(0);
    NodeList vars = rangeElement.getElementsByTagName("Var"); // TODO string constant
    Element var = (Element) constants.item(0);
    if(constant != null){
      return constant.getTextContent();
    }
    if(var != null){
      return var.getTextContent();
    }
    return null; // TODO probably throws an exception
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
    _loopElement.removeChild(_rangeVarElement);
    _loopElement.removeChild(_rangeElement);
  }

  protected void swapRangeElementsWith(CLAWloop otherLoop){
    otherLoop.setNewRange(_rangeVarElement, _rangeElement);
    setNewRange(otherLoop.getRangeVarElement(), otherLoop.getRangeElement());
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


/*
<Var type="Fint" scope="local">i</Var>
<indexRange>
  <lowerBound>
    <FintConstant type="Fint">1</FintConstant>
  </lowerBound>
  <upperBound>
    <FintConstant type="Fint">10</FintConstant>
  </upperBound>
  <step>
    <FintConstant type="Fint">1</FintConstant>
  </step>
</indexRange>
*/

  public String getIterationVariableValue(){
    return _iterationVar;
  }

  public String getLowerBoundValue(){
    return _lowerBoundValue;
  }

  public String getUpperBoundValue(){
    return _upperBoundValue;
  }

  public String getStepValue(){
    return _stepValue;
  }

  public String getFormattedRange(){
    return _iterationVar + "=" + _lowerBoundValue + "," + _upperBoundValue + ","
      + _stepValue;
  }

  public boolean hasSameRangeWith(CLAWloop otherLoop){
    return _lowerBoundValue == otherLoop.getLowerBoundValue() &&
           _upperBoundValue == otherLoop.getUpperBoundValue() &&
           _stepValue == otherLoop.getStepValue() &&
           _iterationVar == otherLoop.getIterationVariableValue();
  }

  public Element getRangeElement(){
    return _rangeElement;
  }

  public Element getRangeVarElement(){
    return _rangeVarElement;
  }


}
