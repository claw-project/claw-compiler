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

  private String it0 = null;
  private String it1 = null;
  private String it2 = null;

  private String newit0 = null;
  private String newit1 = null;
  private String newit2 = null;

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
        if(_loopLevel2 != null){
          System.out.println("  loop 3: " + _loopLevel2.getFormattedRange());
        }
      }

      /* To perform the loop interchange, only the ranges and iteration
       * variables are swapped
       */
      if(_loopLevel1 != null && _loopLevel2 == null){
        // Loop interchange between 2 loops

        // Save inner loop iteration variable and range
        Node tmpIterationVar = _loopLevel1.getRangeVarElement().cloneNode(true);
        Node tmpRange = _loopLevel1.getRangeElement().cloneNode(true);

        // Set the range of loop 0 to loop 1
        _loopLevel1.setNewRange(getRangeVarElement(), getRangeElement());
        // Remove the previous range of loop 1
        _loopLevel1.deleteRangeElements();
        // Set new range of loop 1 to loop 0
        this.setNewRange(tmpIterationVar, tmpRange);
        // Remove the previous range of loop 0
        this.deleteRangeElements();
      } else if (_loopLevel1 != null && _loopLevel2 != null){
        // loop interchange between 3 loops with new-order

        // Save most inner loop iteration variable and range
        Node tmpIterationVar = _loopLevel2.getRangeVarElement().cloneNode(true);
        Node tmpRange = _loopLevel2.getRangeElement().cloneNode(true);

        // Set the range of loop 0 to loop 2
        _loopLevel2.setNewRange(getRangeVarElement(), getRangeElement());
        // Remove the previous range of loop 2
        _loopLevel2.deleteRangeElements();
        // Set new range of loop 2 to loop 0
        this.setNewRange(tmpIterationVar, tmpRange);
        // Remove the previous range of loop 0
        this.deleteRangeElements();

        // recompute the range elements
        _loopLevel2.findRangeElements();
        this.findRangeElements();



        // Save most inner loop iteration variable and range
        tmpIterationVar = _loopLevel2.getRangeVarElement().cloneNode(true);
        tmpRange = _loopLevel2.getRangeElement().cloneNode(true);

        // Set the range of loop 0 to loop 2
        _loopLevel2.setNewRange(_loopLevel1.getRangeVarElement(), _loopLevel1.getRangeElement());
        // Remove the previous range of loop 2
        _loopLevel2.deleteRangeElements();
        // Set new range of loop 2 to loop 0
        _loopLevel1.setNewRange(tmpIterationVar, tmpRange);
        // Remove the previous range of loop 0
        _loopLevel1.deleteRangeElements();

      }
      _transformationDone = true;
    }
  }





  private boolean analyze(){
    Element body = getBodyElement();
    getIterationVariableValue();
    Element loop = findChildLoop(body);
    if(loop == null){
      return false;
    }

    _loopLevel1 = new CLAWloop(_pragmaElement, loop);

    if(_newOrderOption != null){
      String[] vars = _newOrderOption.split(",");

      Element loop1Body = _loopLevel1.getBodyElement();
      Element loop2 = findChildLoop(loop1Body);
      _loopLevel2 = new CLAWloop(_pragmaElement, loop2);
    }


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
