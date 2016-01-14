package x2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xstep represents the step (8.14) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - exprModel TODO move to exprModel in Xbound
 */

public class Xstep extends Xbound {
  public Xstep(Element stepElement){
    super(stepElement);
  }
}
