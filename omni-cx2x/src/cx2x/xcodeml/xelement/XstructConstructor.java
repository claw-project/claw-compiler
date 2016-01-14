package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XstructConstructor represents the FstructConstructor (7.3.1) element in
 * XcodeML intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - exprModel* (XexprModel) TODO
 *
 * Attributes:
 * - Optional:
 *   - type (text)
 */

public class XstructConstructor extends XbaseElement {

  private String _type = null;

  public XstructConstructor(Element baseElement){
    super(baseElement);
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
  }

}
