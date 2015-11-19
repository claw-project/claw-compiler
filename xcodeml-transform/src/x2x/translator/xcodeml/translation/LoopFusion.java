package x2x.translator.xcodeml.translation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class LoopFusion extends Xloop {

  private boolean _merged = false;
  private String _groupLabel = null;

  public LoopFusion(Element pragma, Element loop){
    super(pragma, loop);
    _groupLabel = CLAWpragma.getGroupOptionValue(pragma.getTextContent());
  }

  public String getGroupOptionLabel(){
    return _groupLabel;
  }

  private boolean hasSameParentBlockWith(LoopFusion otherLoop){
    if(_loopElement.getParentNode() == otherLoop.getLoopElement().getParentNode()){
      return true;
    }
    return false;
  }

  private boolean hasSameGroupOption(LoopFusion otherLoop){
    return (otherLoop.getGroupOptionLabel() == null ? getGroupOptionLabel()
      == null : otherLoop.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

  public boolean canMergeWith(LoopFusion other){
    if(!hasSameParentBlockWith(other)){
      return false;
    }
    if(!hasSameGroupOption(other)){
      return false;
    }
    if(!hasSameRangeWith(other)){
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
  public void merge(LoopFusion loop){
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
