/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

/**
 * The XassignStatement represents the FassignStatement (6.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: (Var | FarrayRef | FcharacterRef | FmemberRef | FcoArrayRef)
 * - Required: in base class
 *
 * @author clementval
 */
public class XLValueModel extends XbaseModel {

  /**
   * Constructs a new XLValueModel object from an XbaseElement.
   * @param baseElement The root XbaseElement.
   */
  public XLValueModel(XbaseElement baseElement) {
    super(baseElement);
  }
}
