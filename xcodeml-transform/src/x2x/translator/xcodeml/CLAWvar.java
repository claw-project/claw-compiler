package x2x.translator.xcodeml;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * example of XcodeML representation
 * <Var type="Fint" scope="local">i</Var>
 */

public class CLAWvar {
  private Element _varElement = null;
  private String _identity = null;
  private String _type = null;
  private String _scope = null;

  public CLAWvar(Element var){
    _varElement = var;
  }
}
