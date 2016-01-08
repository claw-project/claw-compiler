package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;


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
 */
public class XifStatement extends XbaseElement {

  Xthen _then = null;

  public XifStatement(Element baseElement){
    super(baseElement);

    _then = XelementHelper.findThen(this);
  }
}
