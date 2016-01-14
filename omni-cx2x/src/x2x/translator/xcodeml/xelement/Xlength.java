package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xlength represents the len (8.6) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel (XexprModel)
 */

public class Xlength extends XbaseElement {

  private XexprModel _exprModel = null;

  public Xlength(Element element){
    super(element);
    _exprModel = XelementHelper.findExprModel(this);
  }

  public XexprModel getExprModel(){
    return _exprModel;
  }
}
