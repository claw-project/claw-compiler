package x2x.translator.xcodeml;

import x2x.translator.pragma.CLAWpragma;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CLAWloopFusion {

  private Element _pragmaElement = null;
  private Element _loopElement = null;
  private boolean _merged = false;
  private String _groupLabel = null;

  public CLAWloopFusion(Element pragma, Element loop){
    _pragmaElement = pragma;
    _loopElement = loop;
    _groupLabel = CLAWpragma.getGroupOptionValue(pragma.getTextContent());
  }

  public Element getLoopElement(){
    return _loopElement;
  }

  public Element getPragmaElement(){
    return _pragmaElement;
  }

  public String getGroupOptionLabel(){
    return _groupLabel;
  }

  private boolean hasSameParentBlockWith(CLAWloopFusion otherLoop){
    if(_loopElement.getParentNode() == otherLoop.getLoopElement().getParentNode()){
      return true;
    }
    return false;
  }

  private boolean hasSameRangeWith(CLAWloopFusion otherLoop){
    return true; // TODO compare range from both element
  }

  private boolean hasSameGroupOption(CLAWloopFusion otherLoop){
    if(otherLoop.getGroupOptionLabel() == null && getGroupOptionLabel() == null){
      return true;
    }

    if(otherLoop.getGroupOptionLabel() == null || getGroupOptionLabel() == null){
      return false;
    }

    if(getGroupOptionLabel().equals(otherLoop.getGroupOptionLabel())){
      return true;
    }
    return false;
  }

  public boolean canMergeWith(CLAWloopFusion otherLoop){
    if(!hasSameParentBlockWith(otherLoop)){
      return false;
    }
    if(!hasSameGroupOption(otherLoop)){
      return false;
    }
    // TODO compare the range !!

    return true;
  }

  public boolean isMerged(){
    return _merged;
  }

  public void finalizeMerge(){
    // Remove the pragma and the loop block of the second loop
    _pragmaElement.getParentNode().removeChild(_pragmaElement);
    _loopElement.getParentNode().removeChild(_loopElement);
    _merged = true;
  }

  /**
   * Merge the given loop with this one
   */
  public void merge(CLAWloopFusion loop){
    NodeList masterBodies = _loopElement.getElementsByTagName("body");
    Element masterBody = (Element) masterBodies.item(0);
    NodeList slaveBodies = loop.getLoopElement().getElementsByTagName("body");
    Element slaveBody = (Element) slaveBodies.item(0);

    // Append content of loop-body (loop) to this loop-body
    for(Node childNode = slaveBody.getFirstChild(); childNode!=null;){
        Node nextChild = childNode.getNextSibling();
        // Do something with childNode, including move or delete...
        if(childNode.getNodeType() == Node.ELEMENT_NODE){
          masterBody.appendChild(childNode);
        }
        childNode = nextChild;
    }
    loop.finalizeMerge();
  }



}
