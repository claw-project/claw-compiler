/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.helper.*;

/**
 * The XdoStatement represents the FdoStatement (6.5) element in XcodeML
 * intermediate representation.
 *
 * Elements: (Var?, indexRange?, body?)
 * - Required:
 *   - Var (Xvar)
 *   - indexRange (XindexRange)
 *   - body (Xbody)
 * Attributes:
 * - Optional: construct_name (text)
 *
 * Can have lineno and file attributes
 *
 * @author clementval
 */

public class XdoStatement extends XbaseElement {
  private XloopIterationRange _iterationRange = null;
  // TODO separate var/indexRange
  private Xbody _body = null;
  private String _construct_name = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XdoStatement(Element baseElement){
    super(baseElement);
    findRangeElements();
    _body = XelementHelper.findBody(this, false);
    _construct_name = XelementHelper.getAttributeValue(this,
        XelementName.ATTR_CONSTRUCT_NAME);
  }

  /**
   * Find the different elements that are included in the iteration range.
   */
  public void findRangeElements(){
    Xvar inductionVar = XelementHelper.findVar(this, false);
    XindexRange indexRange = XelementHelper.findIndexRange(this, false);

    if(inductionVar != null && indexRange != null){
      _iterationRange =
        new XloopIterationRange(inductionVar, indexRange);
    }
  }

  /**
   * Create an empty arrayIndex element in the given program
   */
  public static XdoStatement createEmpty(XcodeProg xcodeml,
    XloopIterationRange range)
  {
    Element element = xcodeml.getDocument().
      createElement(XelementName.DO_STMT);

    if(range != null){
      element.appendChild(range.getInductionVar().cloneNode());
      element.appendChild(range.getIndexRange().cloneNode());
    }

    Element body = xcodeml.getDocument().createElement(XelementName.BODY);
    element.appendChild(body);

    return new XdoStatement(element);
  }

  /**
   * Apply a new iteration range to the do statement.
   * @param range The range to be applied.
   */
  public void setNewRange(XloopIterationRange range){
    Element body = _body.getBaseElement();
    Node newVar = range.getInductionVar().cloneNode();
    Node newRange = range.getIndexRange().cloneNode();
    baseElement.insertBefore(newVar, body);
    baseElement.insertBefore(newRange, body);
    findRangeElements();
  }

  /**
   * Delete the range elements (induction variable and index range).
   */
  public void deleteRangeElements(){
    baseElement.removeChild(_iterationRange.getInductionVar().getBaseElement());
    baseElement.removeChild(_iterationRange.getIndexRange().getBaseElement());
  }

  /**
   * Swap the range elements between two loop statements.
   * @param otherLoop The loop to swap range elements with this one.
   */
  protected void swapRangeElementsWith(XdoStatement otherLoop){
    otherLoop.setNewRange(_iterationRange);
    setNewRange(otherLoop.getIterationRange());
  }

  /**
   * Get the do statement iteration range.
   * @return A XloopIterationRange containing the loop iteration range
   * information.
   */
  public XloopIterationRange getIterationRange(){
    return _iterationRange;
  }

  /**
   * Get the do statement's body.
   * @return A Xbody object for the do statement.
   */
  public Xbody getBody(){
    return _body;
  }

  /**
   * Append the body of a do statement to this one.
   * @param otherLoop The do statement containing the body to be appended.
   * @throws IllegalTransformationException
   */
  public void appendToBody(XdoStatement otherLoop)
    throws IllegalTransformationException
  {
    XelementHelper.appendBody(_body, otherLoop.getBody());
  }

  /**
   * Get the induction variable of the do statement.
   * @return The induction variable as a String value.
   */
  public String getInductionVariable(){
    return _iterationRange.getInductionVar().getValue();
  }

  /**
   * Get the lower bound value of the do statement.
   * @return The lower bound value as a String value.
   */
  public String getLowerBoundValue(){
    return _iterationRange.getIndexRange().getLowerBound().getValue();
  }

  /**
   * Get the upper bound value of the do statement.
   * @return The upper bound value as a String value.
   */
  public String getUpperBoundValue(){
    return _iterationRange.getIndexRange().getUpperBound().getValue();
  }

  /**
   * Get the step value of the do statement.
   * @return The step value as a String value.
   */
  public String getStepValue(){
    return _iterationRange.getIndexRange().getStep().getValue();
  }

  /**
   * @return A string representation of the iterarion range.
   */
  public String getFormattedRange(){
    return _iterationRange.toString();
  }

  /**
   * Check whether two do statements share the same iteration range.
   * @param other The do statement to be compared with the current one.
   * @return True if the two do statements share the same iteration range. False
   * otherwise.
   */
  public boolean hasSameRangeWith(XdoStatement other){
    return _iterationRange.isFullyIdentical(other.getIterationRange());
  }

  /**
   * Check whether the element has a construct name attribute defined.
   * @return True the attribute is defined. False otherwise.
   */
  public boolean hasConstructName(){
    return _construct_name != null;
  }

  /**
   * Get the construct name attribute value.
   * @return Construct name value. Null if the attribute is not defined.
   */
  public String getConstructName(){
    return _construct_name;
  }
}
