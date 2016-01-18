/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

/**
 * A XloopIterationRange represents the elements parts of the iteration in a do
 * statement (induction variable, lower bound, upper bound, step)
 *
 * @author clementval
 */

public class XloopIterationRange {
  private XindexRange _indexRange;
  private Xvar _inductionVariable;

  public XloopIterationRange(Xvar inductionVar, XindexRange range){
    _inductionVariable = inductionVar;
    _indexRange = range;
  }

  public Xvar getInductionVar(){
    return _inductionVariable;
  }

  public XindexRange getIndexRange(){
    return _indexRange;
  }

  /**
   * Compare this loopIterationRange with the given one. Return true if all the
   * internal variable of the loopIterationRange are identical.
   */
  public boolean isFullyIdentical(XloopIterationRange other){
    if(!getInductionVar().getValue().equals(other.getInductionVar().getValue())){
      return false;
    }
    if(!getIndexRange().equals(other.getIndexRange())){
      return false;
    }
    return true;
  }

  /**
   * Get a string representation of the iteration range.
   * @return A string representing the iteration range (e.g. i=1,10,1)
   */
  public String toString() {
    return getInductionVar().getValue() + "="
      + getIndexRange().getLowerBound().getValue() + ","
      + getIndexRange().getUpperBound().getValue() + ","
      + getIndexRange().getStep().getValue();
  }

  /**
   * Clone the current loop iteration range object.
   * @return A new XloopIterationRange that is the clone of the current one.
   */
  public XloopIterationRange cloneObject(){
    Xvar tmpVar = _inductionVariable.cloneObject();
    XindexRange tmpRange = _indexRange.cloneObject();
    return new XloopIterationRange(tmpVar, tmpRange);
  }
}
