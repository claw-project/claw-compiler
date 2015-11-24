package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Map;

/**
 * The XarrayIndex represents the arrayIndex (8.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - exprModel TODO
 */

public class XarrayIndex {
  private Element _element = null;

  public XarrayIndex(Element element){
    _element = element;
  }
}
