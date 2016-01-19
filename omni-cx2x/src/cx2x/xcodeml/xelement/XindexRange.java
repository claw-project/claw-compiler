/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import cx2x.xcodeml.helper.*;

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
 *
 * @author clementval
 */

public class XindexRange extends XbaseElement implements Xclonable<XindexRange> {

  protected XlowerBound _lowerBound;
  protected XupperBound _upperBound;
  protected Xstep _step = null;
  private boolean _isAssumedShape = false;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XindexRange(Element baseElement){
    super(baseElement);
    readElementInformation();
  }

  /**
   * Read inner element information.
   */
  private void readElementInformation(){
    _isAssumedShape = XelementHelper.getBooleanAttributeValue(this,
      XelementName.ATTR_IS_ASSUMED_SHAPE);

    // If the shape is assumed, there is no inner elements
    if(!_isAssumedShape){
      _lowerBound = XelementHelper.findLowerBound(this, false);
      _upperBound = XelementHelper.findUpperBound(this, false);
      _step = XelementHelper.findStep(this, false);
    }
  }

  /**
   * Get the lower bound object.
   * @return XlowerBound object.
   */
  public XlowerBound getLowerBound(){
    return _lowerBound;
  }

  /**
   * Get the upper bound object.
   * @return XupperBound object.
   */
  public XupperBound getUpperBound(){
    return _upperBound;
  }

  /**
   * Check whether the index range has a step defined.
   * @return True if a step is defined. False otherwise.
   */
  public boolean hasStep(){
    return _step != null;
  }

  /**
   * Get the step object.
   * @return Xstep object.
   */
  public Xstep getStep(){
    return _step;
  }

  /**
   * Check whether the index range is of assumed shape.
   * @return True if the index range is of assumed shape.
   */
  public boolean isAssumedShape(){
    return _isAssumedShape;
  }

  /**
   * Clone the current object.
   * @return A new object XindexRange that is the clone of the current object.
   */
  public XindexRange cloneObject(){
    Node clone = cloneNode();
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

    return getStep().equals(other.getStep());

  }

  @Override
  public int hashCode() {
    return _lowerBound.hashCode() ^ _upperBound.hashCode()
      ^ _step.hashCode();
  }
}
