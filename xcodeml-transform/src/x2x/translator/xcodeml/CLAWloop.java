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

    NodeList vars = _loopElement.getElementsByTagName("Var");
    Element var = (Element) vars.item(0);
    _rangeVarElement = var;
    _iterationVar = var.getTextContent();
    NodeList ranges = _loopElement.getElementsByTagName("indexRange");
    Element range = (Element) ranges.item(0);
    _rangeElement = range;

    _lowerBoundValue = getRangeValue("lowerBound");
    _upperBoundValue = getRangeValue("upperBound");
    _stepValue = getRangeValue("step");
  }

  protected void findRangeElements(){
    NodeList vars = _loopElement.getElementsByTagName("Var");
    Element var = (Element) vars.item(0);
    _rangeVarElement = var;
    _iterationVar = var.getTextContent();
    NodeList ranges = _loopElement.getElementsByTagName("indexRange");
    Element range = (Element) ranges.item(0);
    _rangeElement = range;

    _lowerBoundValue = getRangeValue("lowerBound");
    _upperBoundValue = getRangeValue("upperBound");
    _stepValue = getRangeValue("step");
  }

  private String getRangeValue(String tag){
    NodeList rangeElements = _loopElement.getElementsByTagName(tag);
    Element rangeElement = (Element) rangeElements.item(0);
    NodeList constants = rangeElement.getElementsByTagName("FintConstant");
    Element constant = (Element) constants.item(0);
    return constant.getTextContent();
  }

  public void setNewRange(Node var, Node range){
    Element body = getBodyElement();
    Node newVar = var.cloneNode(true);
    Node newRange = range.cloneNode(true);
    _loopElement.insertBefore(newVar, body);
    _loopElement.insertBefore(newRange, body);
    findRangeElements();

    //_rangeVarElement = var;
    //_rangeElement = range;
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
    // TODO be sure that there is only one body per loop
    NodeList bodies = _loopElement.getElementsByTagName("body");
    Element body = (Element) bodies.item(0);
    return body;
  }

  private String getAttributeValue(Element el, String attrName){
    NamedNodeMap attributes = el.getAttributes();
    for (int j = 0; j < attributes.getLength(); j++) {
      if(attributes.item(j).getNodeName().equals(attrName)){
        return attributes.item(j).getNodeValue();
      }
    }
    return "";
  }

  public String getOriginalFilename(){
    return getAttributeValue(_pragmaElement, "file"); // TODO use constant
  }

  public String getPragmaLine(){
    return getAttributeValue(_pragmaElement, "lineno"); // TODO use constant
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
