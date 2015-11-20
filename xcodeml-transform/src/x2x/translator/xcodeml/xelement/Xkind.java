package x2x.translator.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * The Xkind represents the kind (8.1) element in XcodeML intermediate
 * representation.
 * Elements: the base element can contains text data
 */

public class Xkind {
  private Element _element;
  private String _value;

  public Xkind(Element element){
    _element = element;
    _value = _element.getTextContent();
  }

  public String getValue(){
    return _value;
  }

  public void setValue(){
    // TODO
  }

  public Node clone(){
    return _element.cloneNode(true);
  }
}
