package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;

/*
 * Example of XcodeML representation
 *
 * <step>
 *   <Var type="Fint" scope="local">istart</Var>
 * </step>
 */

public class Xstep extends Xbound {
  public Xstep(Element stepElement){
    super(stepElement);
  }
}
