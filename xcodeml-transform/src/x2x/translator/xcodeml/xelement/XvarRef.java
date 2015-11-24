package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The XvarRef represents the varRef (7.4.6) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Required: one of the followings
 *   - Var (Xvar)
 *   - FmemberRef TODO
 *   - FarrayRef (XarrayRef)
 *   - FcharacterRef TODO
 *   - FcoArrayRef TODO (not priority)
 * Attributes:
 * - Optional: type (text) TODO
 */

public class XvarRef {
  private Element _element;

  public XvarRef(Element element){
    _element = element;
    readElementInformation();
  }

  private void readElementInformation(){
    // TODO
  }

  public static Element create(XcodeProg xcodeml, Xvar var, String type, XarrayIndex index){
    /* TODO
      <varRef type="A7fa7b35045b0">
        <Var type="A7fa7b35045b0" scope="local">value1</Var>
      </varRef>
      */
    return null;
  }
}
