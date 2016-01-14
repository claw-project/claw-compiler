package x2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/*
 * Example of XcodeML representation
 *
 * <Var type="Fint" scope="local">i</Var>
 * <indexRange>
 *   <lowerBound>
 *     <Var type="Fint" scope="local">istart</Var>
 *   </lowerBound>
 *   <upperBound>
 *     <Var type="Fint" scope="local">iend</Var>
 *   </upperBound>
 *   <step>
 *     <FintConstant type="Fint">1</FintConstant>
 *   </step>
 * </indexRange>
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

  public String toString() {
    return getInductionVar().getValue() + "="
      + getIndexRange().getLowerBound().getValue() + ","
      + getIndexRange().getUpperBound().getValue() + ","
      + getIndexRange().getStep().getValue();
  }

  public XloopIterationRange cloneObject(){
    Xvar tmpVar = _inductionVariable.cloneObject();
    XindexRange tmpRange = _indexRange.cloneObject();
    return new XloopIterationRange(tmpVar, tmpRange);
  }
}
