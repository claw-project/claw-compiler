/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;

/**
 * The XsymbolTable represents the symbols (4) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - id
 *
 * @author clementval
 */

public class XsymbolTable extends XbaseElement {

  private Hashtable<String, Xid> _table;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XsymbolTable(Element baseElement){
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the symbols table.
   */
  private void readTable(){
    NodeList nodeList = baseElement.getElementsByTagName(XelementName.ID);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if (idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _table.put(id.getName(), id);
      }
    }
  }

  /**
   * Get number of symbols in the table.
   * @return Number of symbols.
   */
  public int count(){
    return _table.size();
  }

  /**
   * Add a new symbols in the table.
   * @param id The new Xid object to be added.
   */
  public void add(Xid id){
    baseElement.appendChild(id.cloneNode());
    _table.put(id.getName(), id);
  }

  /**
   * Get an Xid object in the symbols table based on its name value.
   * @param key Name of the Xid to be returned.
   * @return The Xid object of found. Null otherwise.
   */
  public Xid get(String key){
    if(_table.containsKey(key)){
      return _table.get(key);
    }
    return null;
  }
}
