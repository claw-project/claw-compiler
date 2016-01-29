/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node; 

import java.util.Hashtable;

/**
 * The XglobalDeclTable represents the typeTable (5.1) element in XcodeML
 * intermediate representation.
 *
 * Elements: (FfunctionDefinition | FmoduleDefinition)*
 * - Optional:
 *   - FfunctionDefinition (XfctDef)
 *   - FmoduleDefinition (XmoduleDef) TODO
 *
 * @author clementval
 */

public class XglobalDeclTable extends XbaseElement {

  /*
   * Hastable containing the global declaration elements. Key is the name of the
   * function or the module.
   */
  private Hashtable<String, XbaseElement> _table;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XglobalDeclTable(Element baseElement){
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the declaration table
   */
  private void readTable(){
    Node currentNode = baseElement.getFirstChild();
    while(currentNode != null){
      if (currentNode.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)currentNode;
        if(el.getTagName().equals(XelementName.FCT_DEFINITION)){
          XfctDef fctDef = new XfctDef(el);
          _table.put(fctDef.getFctName(), fctDef);
        } else if(el.getTagName().equals(XelementName.F_MODULE_DEFINITION)){
          // TODO
        }
      }
      currentNode = currentNode.getNextSibling();
    }
  }


  /**
   * Get a specific function declaration based on its name.
   * @param name The name of the function to be returned.
   * @return A XfctDef object if key is found. Null otherwise.
   */
  public XfctDef getFctDefinition(String name){
    if(_table.containsKey(name)) {
      XbaseElement el = _table.get(name);
      if(el instanceof XfctDef){
        return (XfctDef)el;
      }
    }
    return null;
  }

  // TODO getModuleDefinition

  /**
   * Get the number of declarations in the table.
   * @return The number of declarations in the table.
   */
  public int count(){
    return _table.size();
  }

}
