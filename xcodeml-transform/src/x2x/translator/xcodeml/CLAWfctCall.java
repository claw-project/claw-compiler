package x2x.translator.xcodeml;

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

public class CLAWfctCall {
  private Element _fctCallElement = null;
  private CLAWname _fctName = null;

  // TODO move to a single file all XcodeML element constant
  private static final String NAME_ELEMENT = "name";

  public CLAWfctCall(Element fctCallElement){
    _fctCallElement = fctCallElement;
    readElementInformation();
  }

  private void readElementInformation(){
    NodeList names = _fctCallElement.getElementsByTagName(NAME_ELEMENT);
    Element nameElement = (Element) names.item(0);
    _fctName = new CLAWname(nameElement);
  }

  public String getFctName(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getName();
  }

  public String getFctType(){
    if(_fctName == null){
      return null;
    }
    return _fctName.getType();
  }

}
