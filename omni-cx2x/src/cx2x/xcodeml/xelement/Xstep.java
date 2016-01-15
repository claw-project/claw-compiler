/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xstep represents the step (8.14) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel TODO move to exprModel in Xbound
 *
 * @author
 */

public class Xstep extends Xbound {
  public Xstep(Element stepElement){
    super(stepElement);
  }
}
