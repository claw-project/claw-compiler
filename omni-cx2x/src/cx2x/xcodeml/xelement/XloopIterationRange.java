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
  private final XindexRange _indexRange;
  private final Xvar _inductionVariable;

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
   * @param other The other object to be checked with.
   * @return True if the two loop iteration range are identical. False
   * otherwise.
   */
  public boolean isFullyIdentical(XloopIterationRange other){
    if(!getInductionVar().getValue().equals(other.getInductionVar().getValue())){
      return false;
    }
    return getIndexRange().equals(other.getIndexRange());
  }

  public boolean isIdenticalBesidesLowerBound(XloopIterationRange other){
    if(!getInductionVar().getValue().equals(other.getInductionVar().getValue())){
      return false;
    }

    if(getIndexRange().isAssumedShape()
        && other.getIndexRange().isAssumedShape()){
      return true;
    }

    if (!getIndexRange().getUpperBound().equals(
        other.getIndexRange().getUpperBound()))
    {
      return false;
    }

    // step is optional
    return getIndexRange().getStep() == null &&
        other.getIndexRange().getStep() == null ||
        getIndexRange().getStep().equals(other.getIndexRange().getStep());
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
