/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import cx2x.xcodeml.helper.*;

import java.util.ArrayList;
import java.util.List;

/**
 * The XstructConstructor represents the FstructConstructor (7.3.1) element in
 * XcodeML intermediate representation.
 *
 * Elements: ( exprModel )*
 * - Optional:
 *   - exprModel (XexprModel)
 *
 * Attributes:
 * - Optional:
 *   - type (text)
 *
 * @author clementval
 */

public class XstructConstructor extends XbaseElement {

  private String _type = null;
  private List<XexprModel> _models = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XstructConstructor(Element baseElement){
    super(baseElement);
    _type = XelementHelper.getAttributeValue(this,
      XelementName.ATTR_TYPE);
    _models = new ArrayList<>();
    // TODO read all exprModel
  }

  /**
   * Get the struct constructor type.
   * @return Type of the strcut constructor as a String value.
   */
  public String getType(){
    return _type;
  }


  /**
   * Get the list of inner expression models.
   * @return A list of XexprModel objects. 
   */
  public List<XexprModel> getExprModels(){
    return _models;
  }
}
