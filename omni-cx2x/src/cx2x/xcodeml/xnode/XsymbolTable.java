/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import java.util.Hashtable;

/**
 * The XsymbolTable represents the symbols (4) element in XcodeML intermediate
 * representation.
 * <p>
 * Elements: (id*)
 * - Optional:
 * - id
 *
 * @author clementval
 */

public class XsymbolTable extends Xnode {

  private final Hashtable<String, Xid> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XsymbolTable(Element baseElement) {
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the symbols table.
   */
  private void readTable() {
    NodeList nodeList = _baseElement.getElementsByTagName(Xname.ID);
    for(int i = 0; i < nodeList.getLength(); i++) {
      Node idNode = nodeList.item(i);
      if(idNode.getNodeType() == Node.ELEMENT_NODE) {
        Element idElement = (Element) idNode;
        Xid id = new Xid(idElement);
        _table.put(id.getName(), id);
      }
    }
  }

  /**
   * Get number of symbols in the table.
   *
   * @return Number of symbols.
   */
  public int count() {
    return _table.size();
  }

  /**
   * Add a new symbols in the table. Xid object is cloned.
   *
   * @param id The new Xid object to be added.
   */
  public void add(Xid id) {
    add(id, true);
  }

  /**
   * Add a new symbols in the table. Xid object is cloned.
   *
   * @param id    The new Xid object to be added.
   * @param clone Tell whether the element is cloned before added or not. If
   *              true the element is cloned.
   */
  public void add(Xid id, boolean clone) {
    this.append(id, clone);
    _table.put(id.getName(), id);
  }

  /**
   * Get an Xid object in the symbols table based on its name value.
   *
   * @param key Name of the Xid to be returned.
   * @return The Xid object of found. Null otherwise.
   */
  public Xid get(String key) {
    if(_table.containsKey(key)) {
      return _table.get(key);
    }
    return null;
  }

  /**
   * Check if an id is already present in the symbols table.
   *
   * @param id String value of the id to check.
   * @return True if the id is already in the table. False otherwise.
   */
  public boolean contains(String id) {
    return _table.containsKey(id);
  }

  @Override
  public XsymbolTable cloneObject() {
    Element clone = (Element) cloneRawNode();
    return new XsymbolTable(clone);
  }
}
