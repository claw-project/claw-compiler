package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.NamedNodeMap;


/**
 * The XifStatement represents the FifStatement (6.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - condition TODO
 *   - then TODO
 * - Optional:
 *   - else TODO
 * Attributes:
 * - Optional: construct_name (text) TODO
 */
public class XifStatement extends XbaseElement {

  public XifStatement(Element baseElement){
    super(baseElement);
  }
}
