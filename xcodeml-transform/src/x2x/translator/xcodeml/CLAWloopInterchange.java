package x2x.translator.xcodeml;

import x2x.translator.pragma.CLAWpragma;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import xcodeml.util.XmOption;

public class CLAWloopInterchange extends CLAWloop {

  private String _newOrderOption = null;
  private boolean _transformationDone = false;

  private CLAWloop _loopLevel1 = null;
  private CLAWloop _loopLevel2 = null;

  public CLAWloopInterchange(Element pragma, Element loop){
    super(pragma, loop);
    _newOrderOption = CLAWpragma.getNewOrderOptionValue(pragma.getTextContent());
  }

  public void transform(){
    if(analyze()){
      if(XmOption.isDebugOutput()){
        System.out.println("loop-interchange transformation (loop 1 <--> loop 2)");
        System.out.println("  loop 1: " + getFormattedRange());
        System.out.println("  loop 2: " + _loopLevel1.getFormattedRange());
      }
    }


    if(_loopLevel1 != null && _loopLevel2 == null){
      /* Loop interchange between 2 loops
       * To perform the loop interchange, only the ranges and iteration
       * variables are swapped
       */

      String tmpIterationVaribale = _iterationVar;
      String tmpLowerBoundValue = _lowerBoundValue;
      String tmpUpperBoundValue = _upperBoundValue;
      String tmpStepValue = _stepValue;

      // Set range of inner loop to outer loop
      this.setNewRange(_loopLevel1.getIterationVariableValue(),
        _loopLevel1.getLowerBoundValue(), _loopLevel1.getUpperBoundValue(),
        _loopLevel1.getStepValue());

      // Set range of outer loop to inner loop
      _loopLevel1.setNewRange(tmpIterationVaribale, tmpLowerBoundValue,
        tmpUpperBoundValue, tmpStepValue);


    } else if (_loopLevel1 != null && _loopLevel2 != null){
      // loop interchange between 3 loops with new-order
    }






    _transformationDone = true;
  }


  private boolean analyze(){
    Element body = getBodyElement();
    getIterationVariableValue();
    Element loop = findChildLoop(body);
    if(loop == null){
      return false;
    }
    _loopLevel1 = new CLAWloop(_pragmaElement, loop);
    return true;
  }


  private Element findChildLoop(Node from){
    Node nextNode = from.getFirstChild();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("FdoStatement")){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }
}
