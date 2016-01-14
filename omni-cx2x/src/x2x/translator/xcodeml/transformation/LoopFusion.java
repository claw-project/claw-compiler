package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.xcodeml.xelement.*;
import x2x.translator.exception.*;
import x2x.translator.xcodeml.transformer.Transformer;

/**
 * A LoopFusion transformation is a dependent transformation. If two LoopFusion
 * transformation units shared the same fusion group and the same loop iteration
 * information, they can be merge together.
 * The transformation consists of merging the body of the second do statement to
 * the end of the first do statement.
 *
 * @author Valentin Clement
 */

public class LoopFusion extends Transformation<LoopFusion> {
  // Contains the value of the group option
  private String _groupLabel = null;
  // The loop statement involved in the Transformation
  private XdoStatement _loop = null;

  public LoopFusion(Xpragma pragma){
    super(pragma);
    _groupLabel = CLAWpragma.getGroupOptionValue(_pragma.getData());
  }

  public LoopFusion(XdoStatement loop, String group, int lineNumber){
    super(null);
    _loop = loop;
    _groupLabel = group;
    setStartLine(lineNumber);
  }

  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    _loop = XelementHelper.findNextDoStatement(_pragma);
    if(_loop == null){
      xcodeml.addError("Cannot find loop after directive",
        _pragma.getLine());
      return false;
    }
    return true;
  }

  /**
   * Merge the given loop with this one
   */
  public void transform(XcodeProg xcodeml, Transformer transformer,
    LoopFusion loopFusionUnit) throws IllegalTransformationException
  {
    _loop.appendToBody(loopFusionUnit.getLoop());
    loopFusionUnit.finalizeTransformation();
  }

  public void finalizeTransformation(){
    // Delete the pragma of the transformed loop
    if(_pragma != null){
      _pragma.delete();
    }
    // Delete the loop that was merged with the main one
    if(_loop != null){
      _loop.delete();
    }
    this.transformed();
  }

  public boolean canBeTransformedWith(LoopFusion otherLoopUnit){

    // No loop is transformed already
    if(this.isTransformed() || otherLoopUnit.isTransformed()){
      return false;
    }
    // Loops can only be merged if they are at the same level
    if(!XelementHelper.hasSameParentBlock(_loop, otherLoopUnit.getLoop())){
      return false;
    }
    // Loop must share the same group option
    if(!hasSameGroupOption(otherLoopUnit)){
      return false;
    }
    // Loop must share the same iteration range
    if(!_loop.hasSameRangeWith(otherLoopUnit.getLoop())){
      return false;
    }
    return true;
  }

  public XdoStatement getLoop(){
    return _loop;
  }

  public String getGroupOptionLabel(){
    return _groupLabel;
  }

  private boolean hasSameGroupOption(LoopFusion otherLoopUnit){
    return (otherLoopUnit.getGroupOptionLabel() == null
      ? getGroupOptionLabel() == null
      : otherLoopUnit.getGroupOptionLabel().equals(getGroupOptionLabel()));
  }

}
