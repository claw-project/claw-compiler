package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.ArrayList;
import java.util.List;

/**
 * XcodeML AST node.
 *
 * @author clementval
 */
public class Xnode {

  private final Element _baseElement;

  public Xnode(Element element){
    _baseElement = element;
  }

  /**
   * Get the element opcode.
   * @return Opcode.
   */
  public Xcode Opcode(){
    return Xcode.valueOf(_baseElement.getTagName());
  }

  /**
   * Check whether the element has the corresponding attribute.
   * @param attrCode Attribute code.
   * @return True if the element has the corresponding attribute.
   */
  public boolean hasAttribute(Xattr attrCode){
    return _baseElement != null
        && _baseElement.hasAttribute(attrCode.toString());
  }

  /**
   * Get the attribute's value.
   * @param attrCode Attribute code.
   * @return Attribute's value.
   */
  public String getAttribute(Xattr attrCode){
    return _baseElement.getAttribute(attrCode.toString());
  }

  /**
   * Get the element's value.
   * @return Element value.
   */
  public String getValue(){
    return _baseElement.getTextContent();
  }

  /**
   * Set attribute value.
   * @param attrCode Attribute code.
   * @param value Value of the attribute.
   */
  public void setAttribute(Xattr attrCode, String value){
    _baseElement.setAttribute(attrCode.toString(), value);
  }

  /**
   * Get the list of child elements.
   * @return List of children of the current element.
   */
  public List<Xnode> getChildren(){
    List<Xnode> nodes = new ArrayList<>();
    NodeList children = _baseElement.getChildNodes();
    for(int i = 0; i < children.getLength(); ++i){
      Node child = children.item(i);
      if(child.getNodeType() == Node.ELEMENT_NODE){
        nodes.add(new Xnode((Element)child));
      }
    }
    return nodes;
  }

}
