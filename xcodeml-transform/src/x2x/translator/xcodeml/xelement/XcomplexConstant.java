package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import java.util.List;

/**
 * The XcomplexConstant represents the FcomplexConstant elements (7.1.2) element
 * in XcodeML intermediate representation.
 *
 * Elements:
 * - Required: FrealConstant (XrealConstant)
 * Attributes: defined in Xconstant
 * - Optional: type (text)
 */

public class XcomplexConstant extends Xconstant {
  XrealConstant realConst1 = null;
  XrealConstant realConst2 = null;

  public XcomplexConstant(Element element){
    super(element);
    readElementInformation();
  }

  private void readElementInformation(){
    List<XrealConstant> innerElements = XelementHelper
      .getRealConstants(baseElement);
    if(innerElements.size() != 2){
      // TODO error handling : exception
    }
    realConst1 = innerElements.get(0);
    realConst2 = innerElements.get(1);
  }

  public XcomplexConstant create(XcodeProg xcodeml, String value1, String type1,
    String kind1, String value2, String type2, String kind2)
  {
    if(value1 == null || value2 == null){
      return null;
    }
    Element base = xcodeml.getDocument().
      createElement(XelementName.F_COMPLEX_CONST);

    Xconstant constant1 = create(xcodeml, value1, type1, kind1, XconstType.REAL);
    Xconstant constant2 = create(xcodeml, value2, type2, kind2, XconstType.REAL);

    base.appendChild(constant1.getBaseElement());
    base.appendChild(constant2.getBaseElement());

    return new XcomplexConstant(base);
  }
}
