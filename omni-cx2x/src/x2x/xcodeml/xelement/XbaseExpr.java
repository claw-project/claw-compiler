package x2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XbaseExpr is a base class for the following elements
 *
 * Elements:
 * - Required: exprModel (XexprModel), exprModel (XexprModel)
 */

public class XbaseExpr extends XbaseElement {
  private XexprModel _part1 = null;
  private XexprModel _part2 = null;

  public XbaseExpr(Element element){
    super(element);
  }
}
