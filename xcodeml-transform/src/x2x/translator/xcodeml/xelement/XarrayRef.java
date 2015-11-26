package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;


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
 *   - FarrayRef TODO (XarrayRef)
 * Attributes:
 * - Optional: type (text)
 */

public class XarrayRef extends XbaseElement {

  public XarrayRef(Element arrayRefElement){
    super(arrayRefElement);
    readElementInformation();
  }

  private void readElementInformation(){

  }

  public static Element create(XcodeProg xcodeml, Xvar var, String type, XarrayIndex index){
    // Make sure var is an array
    // TODO

    // Wrap the var with the array reference
    Node varClone = var.clone();
    Element arrayRef = xcodeml.getDocument().createElement(XelementName.F_ARRAY_REF);
    if(type != null){ // Set ref type of the Xvar
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    Element varRef = xcodeml.getDocument().createElement(XelementName.VAR_REF);
    varRef.setAttribute(XelementName.ATTR_TYPE, var.getType());
    varRef.appendChild(varClone);
    arrayRef.appendChild(varRef);
    arrayRef.appendChild(index.getBaseElement());


    /*<Var type="A7fa7b35045b0" scope="local">value1</Var>

    Create this element
    <FarrayRef type="Fint">
      <varRef type="A7fa7b35045b0">
        <Var type="A7fa7b35045b0" scope="local">value1</Var>
      </varRef>
      <arrayIndex>
        <Var type="Fint" scope="local">i</Var>
      </arrayIndex>
    </FarrayRef>
    */



    return arrayRef;
  }
}
