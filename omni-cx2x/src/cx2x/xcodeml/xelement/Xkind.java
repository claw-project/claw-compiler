/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;

/**
 * The Xkind represents the kind (8.1) element in XcodeML intermediate
 * representation.
 * Elements: the base element can contains text data
 *
 * @author clementval
 */

public class Xkind extends XbaseElement {
  private String _value;

  public Xkind(Element kindElement){
    super(kindElement);
    _value = baseElement.getTextContent();
  }

  public String getValue(){
    return _value;
  }

  public void setValue(){
    // TODO
  }
}
