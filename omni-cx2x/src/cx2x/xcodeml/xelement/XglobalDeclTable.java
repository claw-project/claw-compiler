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
 *   - FfunctionDefinition (XfunctionDefinition)
 *   - FmoduleDefinition (XmoduleDefinition)
 *
 * @author clementval
 */

public class XglobalDeclTable extends XbaseElement
    implements Xclonable<XglobalDeclTable>
{

  /*
   * Hastable containing the global declaration elements. Key is the name of the
   * function or the module.
   */
  private final Hashtable<String, XbaseElement> _table;

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
          XfunctionDefinition fctDef = new XfunctionDefinition(el);
          _table.put(fctDef.getName().getValue(), fctDef);
        } else if(el.getTagName().equals(XelementName.F_MODULE_DEFINITION)){
          XmoduleDefinition moduleDef = new XmoduleDefinition(el);
          _table.put(moduleDef.getName(), moduleDef);
        }
      }
      currentNode = currentNode.getNextSibling();
    }
  }

  /**
   * Get a specific function declaration based on its name.
   * @param name The name of the function to be returned.
   * @return A XfunctionDefinition object if key is found. Null otherwise.
   */
  public XfunctionDefinition getFctDefinition(String name){
    if(_table.containsKey(name)) {
      XbaseElement el = _table.get(name);
      if(el instanceof XfunctionDefinition){
        return (XfunctionDefinition)el;
      }
    }
    return null;
  }

  /**
   * Get a specific module declaration based on its name.
   * @param name The name of the module to be returned.
   * @return A XmoduleDefinition object if key is found. Null otherwise.
   */
  public XmoduleDefinition getModuleDefinition(String name){
    if(_table.containsKey(name)){
      XbaseElement el = _table.get(name);
      if(el instanceof XmoduleDefinition){
        return  (XmoduleDefinition)el;
      }
    }
    return null;
  }

  /**
   * Check if there is a definition for the given name.
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasDefinition(String name){
    return _table.containsKey(name);
  }

  /**
   * Check if there is a module definition for the given name.
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasModuleDefinition(String name) {
    return _table.containsKey(name) &&
        (_table.get(name) instanceof XmoduleDefinition);
  }

  /**
   * Check if there is a function definition for the given name.
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasFunctionDefinition(String name){
    return _table.containsKey(name) &&
        (_table.get(name) instanceof XfunctionDefinition);
  }

  /**
   * Get the number of declarations in the table.
   * @return The number of declarations in the table.
   */
  public int count(){
    return _table.size();
  }


  @Override
  public XglobalDeclTable cloneObject() {
    Element clone = (Element)cloneNode();
    return new XglobalDeclTable(clone);
  }

}
