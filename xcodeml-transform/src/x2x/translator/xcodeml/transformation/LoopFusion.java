package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformer.Transformer;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


public class LoopFusion implements Transformation<LoopFusion> {

  private boolean _transformed = false;
  // Contains the value of the group option
  private String _groupLabel = null;
  // The pragma statement that triggered the transformation
  private Xpragma _loopFusionPragma = null;
  // The loop statement involved in the Transformation
  private XdoStatement _loop = null;

  private int _startLine = 0;

  public LoopFusion(Xpragma pragma){
    _loopFusionPragma = pragma;
    _groupLabel = CLAWpragma.getGroupOptionValue(_loopFusionPragma.getData());
  }

  public LoopFusion(XdoStatement loop, String group){
    _loop = loop;
    _groupLabel = group;
  }

  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    Element loopElement =
      XelementHelper.findNextLoop(_loopFusionPragma.getBaseElement());

    if(loopElement == null){
      // TODO give the reason and stops analysis
      return false;
    }
    _loop = new XdoStatement(loopElement);
    return true;
  }

  /**
   * Merge the given loop with this one
   */
  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopFusion loopFusionUnit)
  {
    XelementHelper.appendBody(_loop.getBaseElement(),
      loopFusionUnit.getLoop().getBaseElement());
    loopFusionUnit.finalizeTransformation();
  }

  public boolean isTransformed(){
    return _transformed;
  }

  public boolean canBeTransformedWith(LoopFusion otherLoopUnit){
    System.out.println("  Compare " + getGroupOptionLabel() + "/" +
      otherLoopUnit.getGroupOptionLabel());

    // No loop is transformed already
    if(_transformed || otherLoopUnit.isTransformed()){
      System.out.println("  one transformed");
      return false;
    }
    // Loops can only be merged if they are at the same level
    if(!hasSameParentBlockWith(otherLoopUnit)){
      System.out.println("  different parent block");
      return false;
    }
    // Loop must share the same group option
    if(!hasSameGroupOption(otherLoopUnit)){
      System.out.println("  Different group option");
      return false;
    }
    // Loop must share the same iteration range
    if(!_loop.hasSameRangeWith(otherLoopUnit.getLoop())){
      System.out.println("  Different range " + _loop.getIterationRange().toString() +  " / " + otherLoopUnit.getLoop().getIterationRange().toString());
      return false;
    }
    return true;
  }

  public void finalizeTransformation(){
    // Delete the pragma of the transformed loop
    if(_loopFusionPragma != null){
      _loopFusionPragma.delete();
    }
    // Delete the loop that was merged with the main one
    if(_loop != null){
      _loop.delete();
    }
    _transformed = true;
  }

  public XdoStatement getLoop(){
    return _loop;
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

  public int getStartLine(){
    return _startLine;
  }

}
