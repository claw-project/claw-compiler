package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;

/**
 * The XarrayRef represents the FarrayRef (7.4.4) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Required:
 *   - varRef
 * - Optional:
 *   - arrayIndex TODO
 *   - indexRange TODO
 *   - FarrayConstructor TODO
 *   - FarrayRef TODO
 * Attributes:
 * - Optional: type (text)
 */

public class XarrayRef {
  private Element _element;

  public XarrayRef(Element element){
    _element = element;
    readElementInformation();
  }

  public XarrayRef(Xvar var, String type){
    /*<Var type="A7fa7b35045b0" scope="local">value1</Var>
    <FarrayRef type="Fint">
      <varRef type="A7fa7b35045b0">
        <Var type="A7fa7b35045b0" scope="local">value1</Var>
      </varRef>
      <arrayIndex>
        <FintConstant type="Fint">1</FintConstant>
      </arrayIndex>
    </FarrayRef>*/

  }

  private void readElementInformation(){

  }
}
