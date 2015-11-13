package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Class representing an xcodeml indexRange
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

public class CLAWindexRange {

  protected CLAWbound _lowerBound;
  protected CLAWbound _upperBound;
  protected CLAWbound _step;

  protected Element _indexRangeElement;

  public CLAWindexRange(CLAWbound lowerBound, CLAWbound upperBound, CLAWbound step){
      _lowerBound = lowerBound;
      _upperBound = upperBound;
      _step = step;
  }

  public CLAWindexRange(Element indexRangeElement){
    _indexRangeElement = indexRangeElement;
    readRangeValue();
  }

  private void readRangeValue(){
    _lowerBound = new CLAWbound(CLAWelementHelper
      .findFirstElement(_indexRangeElement, XelementName.LOWER_BOUND));
    _upperBound = new CLAWbound(CLAWelementHelper
      .findFirstElement(_indexRangeElement,XelementName.UPPER_BOUND));
    _step = new CLAWbound(CLAWelementHelper
      .findFirstElement(_indexRangeElement,XelementName.STEP));
  }

  public CLAWbound getLowerBound(){
    return _lowerBound;
  }

  public CLAWbound getUpperBound(){
    return _upperBound;
  }

  public CLAWbound getStep(){
    return _step;
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    CLAWindexRange other = (CLAWindexRange)ob;

    if (!getLowerBound().equals(other.getLowerBound())){
      return false;
    }

    if (!getUpperBound().equals(other.getUpperBound())){
      return false;
    }

    if (!getStep().equals(other.getStep())){
      return false;
    }

    return true;
  }

  @Override
  public int hashCode() {
    return _lowerBound.hashCode() ^ _upperBound.hashCode()
      ^ _step.hashCode();
  }
}
