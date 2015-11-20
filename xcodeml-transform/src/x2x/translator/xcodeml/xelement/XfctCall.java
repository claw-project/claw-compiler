package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * The XfctCall represents the functionCall (7.5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - name (Xname)
 * - Optional:
 *   - arguments (XexprModel) // TODO
 * Attribute:
 * - Optional: type (text), is_intrinsic (bool) // TODO
 */

public class XfctCall extends Xfct {

  public XfctCall(Element fctCallElement){
    super(fctCallElement);
  }
}
