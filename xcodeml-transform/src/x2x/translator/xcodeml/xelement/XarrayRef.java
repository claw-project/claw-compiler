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

  private String _type = null;
  private Xvar _var = null;

  public XarrayRef(Element arrayRefElement){
    super(arrayRefElement);
    readElementInformation();
  }

  private void readElementInformation(){
    // TODO

    _type = XelementHelper.getAttributeValue(baseElement
      , XelementName.ATTR_TYPE);

    // Find Var element
    Element varElement = XelementHelper.findVar(baseElement);
    if(varElement != null){
      _var = new Xvar(varElement);
    }

  }

  public Xvar getVar(){
    return _var;
  }

  public void append(XbaseElement element){
    append(element, false);
  }

  public void append(XbaseElement element, boolean cloneElement){
    if(cloneElement){
      Node clone = element.clone();
      baseElement.appendChild(clone);
    } else {
      baseElement.appendChild(element.getBaseElement());
    }

    if(element instanceof Xvar){
      _var = (Xvar)element;
    }
  }

  /**
   * Create an empty arrayReg element in the given program
   * param type attribute of the element. If null, no attribute is set
   */
  public static XarrayRef createEmpty(XcodeProg xcodeml, String type){
    Element arrayRef = xcodeml.getDocument().
      createElement(XelementName.F_ARRAY_REF);
    if(type != null){
      arrayRef.setAttribute(XelementName.ATTR_TYPE, type);
    }
    return new XarrayRef(arrayRef);
  }



  /*public static Element create(XcodeProg xcodeml, Xvar var, String type, XarrayIndex index){
    // Make sure var is an array
    // TODO

    // Wrap the var with the array reference


    /*Element varRef = xcodeml.getDocument().createElement(XelementName.VAR_REF);
    varRef.setAttribute(XelementName.ATTR_TYPE, var.getType());
    varRef.appendChild(varClone);
    arrayRef.appendChild(varRef);
    arrayRef.appendChild(index.getBaseElement());*/


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




    return arrayRef;
  }*/
}
