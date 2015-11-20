package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The XindexRange represents the indexRange (8.11) element in XcodeML
 * intermediate representation.
 * 
 * Elements:
 * - Optional:
 *   - lowerBound (Xbound)
 *   - upperBound (Xbound)
 *   - step (Xstep)
 * Attributes:
 * - Optional: is_assume_size (bool) // TODO
 */
public class XindexRange {

  protected Xbound _lowerBound;
  protected Xbound _upperBound;
  protected Xstep _step = null;

  protected Element _indexRangeElement;

  public XindexRange(Xbound lowerBound, Xbound upperBound, Xstep step){
      _lowerBound = lowerBound;
      _upperBound = upperBound;
      _step = step;
  }

  public XindexRange(Element indexRangeElement){
    _indexRangeElement = indexRangeElement;
    readRangeValue();
  }

  public Node clone(){
    return _indexRangeElement.cloneNode(true);
  }

  private void readRangeValue(){
    _lowerBound = new Xbound(XelementHelper
      .findFirstElement(_indexRangeElement, XelementName.LOWER_BOUND));
    _upperBound = new Xbound(XelementHelper
      .findFirstElement(_indexRangeElement,XelementName.UPPER_BOUND));
    Element step = XelementHelper
      .findFirstElement(_indexRangeElement, XelementName.STEP);
    if(step != null){
      _step = new Xstep(step);
    }
  }

  public Xbound getLowerBound(){
    return _lowerBound;
  }

  public Xbound getUpperBound(){
    return _upperBound;
  }

  public boolean hasStep(){
    return _step != null;
  }

  public Xbound getStep(){
    return _step;
  }

  @Override
  public boolean equals(Object ob) {
    if (ob == null) return false;
    if (ob.getClass() != getClass()) return false;
    XindexRange other = (XindexRange)ob;

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
