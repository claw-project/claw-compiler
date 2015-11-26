package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class LoopFusion extends XdoStatement implements Transformation<LoopFusion> {

  private boolean _transformed = false;
  private String _groupLabel = null;

  public LoopFusion(Element pragma, Element loop){
    super(pragma, loop);
    _groupLabel = CLAWpragma.getGroupOptionValue(pragma.getTextContent());
  }

  public boolean analyze(XcodeProg xcodeml) {
    return true;
  }


  public String getGroupOptionLabel(){
    return _groupLabel;
  }

  private boolean hasSameParentBlockWith(LoopFusion otherLoop){
    if(baseElement.getParentNode() == otherLoop.getBaseElement().getParentNode()){
      return true;
    }
    return false;
  }

  private boolean hasSameGroupOption(LoopFusion otherLoop){
    return (otherLoop.getGroupOptionLabel() == null ? getGroupOptionLabel()
      == null : otherLoop.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

  public boolean canBeTransformedWith(LoopFusion other){
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

  public boolean isTransformed(){
    return _transformed;
  }


  public void finalizeTransformation(){
    // Remove the pragma and the loop block of the second loop
    XelementHelper.delete(getPragma().getBaseElement());
    XelementHelper.delete(baseElement);
    _transformed = true;
  }

  /**
   * Merge the given loop with this one
   */
  public void transform(XcodeProg xcodeml, LoopFusion loop){
    XelementHelper.appendBody(baseElement, loop.getBaseElement());
    loop.finalizeTransformation();
  }

}
