/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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
   * @param node Raw node.
   */
  public XglobalDeclTable(Xnode node) {
    super(node == null ? null : node.element());
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the declaration table
   */
  private void readTable() {
    Xnode crt = firstChild();
    while(crt != null) {
      if(crt.opcode() == Xcode.FFUNCTIONDEFINITION) {
        XfunctionDefinition fctDef = new XfunctionDefinition(crt);
        _table.put(fctDef.getName(), fctDef);
      } else if(crt.opcode() == Xcode.FMODULEDEFINITION) {
        XmoduleDefinition moduleDef = new XmoduleDefinition(crt);
        _table.put(moduleDef.getName(), moduleDef);
      }
      crt = crt.nextSibling();
    }
  }

  /**
   * Get the number of declarations in the table.
   *
   * @return The number of declarations in the table.
   */
  public int size() {
    return _table.size();
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
   * Retrieve function definition in the current declaration table or
   * recursively in the modules' declaration tables.
   *
   * @param fctName Function's name.
   * @return The function definition if found. Null otherwise.
   */
  public XfunctionDefinition getFunctionDefinition(String fctName) {
    if(_table.containsKey(fctName)) {
      Xnode el = _table.get(fctName);
      if(el instanceof XfunctionDefinition) {
        return (XfunctionDefinition) el;
      }
    } else {
      for(Map.Entry<String, Xnode> entry : _table.entrySet()) {
        if(entry.getValue() instanceof XmoduleDefinition) {
          XmoduleDefinition mod = (XmoduleDefinition) entry.getValue();
          XfunctionDefinition fctDef = mod.getFunctionDefinition(fctName);
          if(fctDef != null) {
            return fctDef;
          }
        }
      }
    }
    return null;
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
   * Check if there is a definition for the given name.
   *
   * @param name Name to be searched.
   * @return True if there is a definition. False otherwise.
   */
  public boolean hasDefinition(String name) {
    return _table.containsKey(name);
  }

  @Override
  public XglobalDeclTable cloneNode() {
    return new XglobalDeclTable(super.cloneNode());
  }
}
