package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class LoopFusion implements Transformation<LoopFusion> {

  private boolean _transformed = false;
  private String _groupLabel = null;
  private Xpragma _loopFusionPragma = null;
  private XdoStatement _loop = null;

  public LoopFusion(Xpragma pragma){
    _loopFusionPragma = pragma;
    _groupLabel = CLAWpragma.getGroupOptionValue(_loopFusionPragma.getData());
  }

  public XdoStatement getLoop(){
    return _loop;
  }

  public boolean analyze(XcodeProg xcodeml) {
    Element loopElement =
      XelementHelper.findNextLoop(_loopFusionPragma.getBaseElement());

    if(loopElement == null){
      // TODO give the reason and stops analysis
      return false;
    }
    _loop = new XdoStatement(loopElement);
    return true;
  }

  public String getGroupOptionLabel(){
    return _groupLabel;
  }

  private boolean hasSameParentBlockWith(LoopFusion otherLoopUnit){
    if(_loop.getBaseElement().getParentNode()
      == otherLoopUnit.getLoop().getBaseElement().getParentNode())
    {
      return true;
    }
    return false;
  }

  private boolean hasSameGroupOption(LoopFusion otherLoopUnit){
    return (otherLoopUnit.getGroupOptionLabel() == null
      ? getGroupOptionLabel() == null
      : otherLoopUnit.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

  public boolean canBeTransformedWith(LoopFusion other){
    if(!hasSameParentBlockWith(other)){
      return false;
    }
    if(!hasSameGroupOption(other)){
      return false;
    }
    if(!_loop.hasSameRangeWith(other.getLoop())){
      return false;
    }
    return true;
  }

  public boolean isTransformed(){
    return _transformed;
  }


  public void finalizeTransformation(){
    // Remove the pragma and the loop block of that was loop
    _loopFusionPragma.delete();
    _loop.delete();
    _transformed = true;
  }

  /**
   * Merge the given loop with this one
   */
  public void transform(XcodeProg xcodeml, LoopFusion loopFusionUnit){
    XelementHelper.appendBody(_loop.getBaseElement(),
      loopFusionUnit.getLoop().getBaseElement());
    loopFusionUnit.finalizeTransformation();
  }

}
