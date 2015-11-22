package x2x.translator.xcodeml.translation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

// TODO refactor to adpot Translation interface
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
    XelementHelper.delete(_pragmaElement);
    XelementHelper.delete(_loopElement);
    _merged = true;
  }

  /**
   * Merge the given loop with this one
   */
  public void merge(LoopFusion loop){
    XelementHelper.appendBody(_loopElement, loop.getLoopElement());
    loop.finalizeMerge();
  }



}
