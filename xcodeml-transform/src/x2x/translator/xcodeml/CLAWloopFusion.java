package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class CLAWloopFusion {

  private Element _pragmaElement = null;
  private Element _loopElement = null;
  private boolean _merged = false;

  public CLAWloopFusion(Element pragma, Element loop){
    _pragmaElement = pragma;
    _loopElement = loop;
  }



  public Element getLoopElement(){
    return _loopElement;
  }

  public Element getPragmaElement(){
    return _pragmaElement;
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

  public boolean canMergeWith(CLAWloopFusion otherLoop){
    if(!hasSameParentBlockWith(otherLoop)){
      return false;
    }
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
