/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import cx2x.xcodeml.helper.*;

/**
 * The XargumentsTable represents the arguments (7.5.3) element in XcodeML
 * intermediate representation.
 *
 * Elements:
 * - Optional:
 *   - exprModel
 *
 * @author clementval
 */

public class XargumentsTable extends XbaseElement {
  // TODO move to exprModel. For the moment only read var
  private Map<String, XbaseElement> _table;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XargumentsTable(Element baseElement){
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read all the arguments present in the arguments table
   */
  private void readTable(){
    // Read Var element
    NodeList elements = baseElement.getElementsByTagName(XelementName.VAR);
    for (int i = 0; i < elements.getLength(); i++) {
      Node n = elements.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        Xvar var = new Xvar(el); // TODO move to exprModel
        _table.put(var.getValue(), var);
      }
    }
  }

  /**
   * Find a specific arguments.
   * @param name Name of the arguments.
   * @return The argument if found. Null otherwise.
   */
  public XbaseElement findArgument(String name){
    if(_table.containsKey(name)) {
      return _table.get(name);
    }
    return null;
  }

  /**
   * Replace a var arguments by a array reference.
   * @param var       The var arguments to be replaced.
   * @param arrayRef  The new array reference arguments.
   */
  public void replace(Xvar var, XarrayRef arrayRef){
    if(var != null){
      XelementHelper.insertAfter(var, arrayRef);
      var.delete();
    } else {
      // TODO trigger a critical error
    }
  }

  /**
   * Add an arrayRef to the arguments table.
   * @param arrayRef The arrayRef to be added.
   */
  public void add(XarrayRef arrayRef){
    baseElement.appendChild(arrayRef.cloneNode());
    //_table.put(arrayRef.getVar().getValue(), arrayRef); TODO
  }

  /**
   * Add a var to the arguments table.
   * @param var The var to be added.
   */
  public void add(Xvar var){
    baseElement.appendChild(var.cloneNode());
    _table.put(var.getValue(), var);
  }

  /**
   * Get an argument element based on its key value.
   * @param key The key to search for the arguments.
   * @return The derived XbaseElement if found. Null otherwiese.
   */
  public XbaseElement get(String key){
    // TODO check if present otherwise null.
    return _table.get(key);
  }

  /**
   * Get an iterator on the arguments table
   * @return An iterator over the arguments table
   */
  public Iterator<Map.Entry<String, XbaseElement>> iterator(){
    return _table.entrySet().iterator();
  }
}
