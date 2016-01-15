/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xbody represents the body (8.7) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - statementModel*
 *
 * @author clementval
 */

public class Xbody extends XbaseElement {

  public Xbody(Element element){
    super(element);
  }
}
