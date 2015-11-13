package x2x.translator.xcodeml;

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

public class CLAWloopIterationRange {
  private CLAWindexRange _indexRange;
  private CLAWvar _inductionVariable;

  public CLAWloopIterationRange(Element inductionVarElement, Element indexRangeElement){
    _inductionVariable = new CLAWvar(inductionVarElement);
    _indexRange = new CLAWindexRange(indexRangeElement);
  }

  public CLAWvar getInductionVar(){
    return _inductionVariable;
  }

  public CLAWindexRange getIndexRange(){
    return _indexRange;
  }

  /**
   * Compare this loopIterationRange with the given one. Return true if all the
   * internal variable of the loopIterationRange are identical.
   */
  public boolean isFullyIdentical(CLAWloopIterationRange other){
    if(!getInductionVar().equals(other.getInductionVar())){
      return false;
    }
    if(!getIndexRange().equals(other.getIndexRange())){
      return false;
    }
    return true;
  }
}
