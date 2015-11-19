package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/*
 * Example of XcodeML representation
 *
 * <functionCall type="Fvoid">
 *  <name type="F7fdd2b600350">clawloop</name>
 *  <arguments>
 *    <Var type="A7fdd2b5046a0" scope="local">value1</Var>
 *    <Var type="A7fdd2b504f50" scope="local">value2</Var>
 *   </arguments>
 * </functionCall>
 */

public class XfctCall extends Xfct {

  public XfctCall(Element fctCallElement){
    super(fctCallElement);
  }
}
