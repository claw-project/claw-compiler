/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The XbaseExpr is a base class for the following elements
 *
 * Elements:
 * - Required: exprModel (XexprModel), exprModel (XexprModel)
 *
 * @author clementval
 */

public class XbaseExpr extends XbaseElement {
  private XexprModel _part1 = null;
  private XexprModel _part2 = null;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XbaseExpr(Element baseElement){
    super(baseElement);
  }
}
