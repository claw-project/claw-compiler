package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * The XindexRange represents the indexRange (8.11) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - lowerBound (XlowerBound)
 *   - upperBound (XupperBound)
 *   - step (Xstep)
 * Attributes:
 * - Optional: is_assumed_shape (bool)
 */
public class XindexRange extends XbaseElement implements Xclonable<XindexRange> {

  protected XlowerBound _lowerBound;
  protected XupperBound _upperBound;
  protected Xstep _step = null;
  private boolean _isAssumedShape = false;

  public XindexRange(Element indexRangeElement){
    super(indexRangeElement);
    readRangeValue();
  }

  private void readRangeValue(){
    _isAssumedShape = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_ASSUMED_SHAPE);

    // If the shape is assumed, there is no inner elements
    if(!_isAssumedShape){
      _lowerBound = XelementHelper.findLowerBound(this, false);
      _upperBound = XelementHelper.findUpperBound(this, false);
      _step = XelementHelper.findStep(this, false);
    }
  }

  public XlowerBound getLowerBound(){
    return _lowerBound;
  }

  public XupperBound getUpperBound(){
    return _upperBound;
  }

  public boolean hasStep(){
    return _step != null;
  }

  public Xstep getStep(){
    return _step;
  }

  public boolean isAssumedShape(){
    return _isAssumedShape;
  }

  public XindexRange cloneObject(){
    Node clone = clone();
    return new XindexRange((Element)clone);
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
