/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.Hashtable;
import java.util.Map;

/**
 * The XglobalDeclTable represents the typeTable (5.1) element in XcodeML
 * intermediate representation.
 * <p>
 * Elements: (FfunctionDefinition | FmoduleDefinition)*
 * - Optional:
 * - FfunctionDefinition (XfunctionDefinition)
 * - FmoduleDefinition (XmoduleDefinition)
 *
 * @author clementval
 */

public class XglobalDeclTable extends Xnode {

  /* Hashtable containing the global declaration elements. Key is the name of
   * the function or the module. */
  private final Hashtable<String, Xnode> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XglobalDeclTable(Element baseElement) {
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the declaration table
   */
  private void readTable() {
    Node currentNode = _baseElement.getFirstChild();
    while(currentNode != null) {
      if(currentNode.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) currentNode;
        if(el.getTagName().equals(Xname.F_FUNCTION_DEFINITION)) {
          XfunctionDefinition fctDef = new XfunctionDefinition(el);
          _table.put(fctDef.getName().value(), fctDef);
        } else if(el.getTagName().equals(Xname.F_MODULE_DEFINITION)) {
          XmoduleDefinition moduleDef = new XmoduleDefinition(el);
          _table.put(moduleDef.getName(), moduleDef);
        }
      }
      currentNode = currentNode.getNextSibling();
    }
  }

  /**
   * Get a specific function declaration based on its name.
   *
   * @param name The name of the function to be returned.
   * @return A XfunctionDefinition object if key is found. Null otherwise.
   */
  public XfunctionDefinition getFctDefinition(String name) {
    if(_table.containsKey(name)) {
      Xnode el = _table.get(name);
      if(el instanceof XfunctionDefinition) {
        return (XfunctionDefinition) el;
      }
    }
    return null;
  }

  /**
   * Get a specific module declaration based on its name.
   *
   * @param name The name of the module to be returned.
   * @return A XmoduleDefinition object if key is found. Null otherwise.
   */
  public XmoduleDefinition getModuleDefinition(String name) {
    if(_table.containsKey(name)) {
      Xnode el = _table.get(name);
      if(el instanceof XmoduleDefinition) {
        return (XmoduleDefinition) el;
      }
    }
    return null;
  }

  /**
   * Check if there is a definition for the given name.
   *
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasDefinition(String name) {
    return _table.containsKey(name);
  }

  /**
   * Check if there is a module definition for the given name.
   *
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasModuleDefinition(String name) {
    return _table.containsKey(name) &&
        (_table.get(name) instanceof XmoduleDefinition);
  }

  /**
   * Check if there is a function definition for the given name.
   *
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasFunctionDefinition(String name) {
    return _table.containsKey(name) &&
        (_table.get(name) instanceof XfunctionDefinition);
  }

  /**
   * Get the number of declarations in the table.
   *
   * @return The number of declarations in the table.
   */
  public int count() {
    return _table.size();
  }

  /**
   * Retrieve function definition in the current declaration table or
   * recursively in the modules' declaration tables.
   *
   * @param fctName Function's name.
   * @return The function definition if found. Null otherwise.
   */
  public XfunctionDefinition getFunctionDefinition(String fctName) {
    for(Map.Entry<String, Xnode> entry : _table.entrySet()) {
      if(entry.getValue() instanceof XmoduleDefinition) {
        XmoduleDefinition mod = (XmoduleDefinition) entry.getValue();
        XfunctionDefinition fctDef = mod.getFunctionDefinition(fctName);
        if(fctDef != null) {
          return fctDef;
        }
      } else if(entry.getValue() instanceof XfunctionDefinition) {
        XfunctionDefinition fctDef = (XfunctionDefinition) entry.getValue();
        if(fctDef.getName().value().equals(fctName)) {
          return fctDef;
        }
      }
    }
    return null;
  }

  @Override
  public XglobalDeclTable cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new XglobalDeclTable(clone);
  }

}
