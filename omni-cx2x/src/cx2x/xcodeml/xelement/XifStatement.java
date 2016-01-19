/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;


/**
 * The XifStatement represents the FifStatement (6.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - condition TODO
 *   - then (Xthen)
 * - Optional:
 *   - else TODO
 * Attributes:
 * - Optional: construct_name (text) TODO
 *
 * @author clementval
 */

public class XifStatement extends XbaseElement {


  private Xthen _then = null;
  private Xelse _else = null;

  // attributes
  private String _constructName = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XifStatement(Element baseElement){
    super(baseElement);

    _then = XelementHelper.findThen(this, false);
    _else = XelementHelper.findElse(this, false);

    // read optional attributes
    _constructName = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_CONSTRUCT_NAME);
  }
}
