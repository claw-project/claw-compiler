/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

import java.util.*;

/**
 * The XtypeTable represents the typeTable (3.1) element in XcodeML intermediate
 * representation.
 * <p>
 * Elements: ( FbasicType | FfunctionType | FstructType ) *
 * - Optional:
 * - FbasicType (XbasicType)
 * - FfunctionType (XfunctionType)
 * - FstructType (XstructType)
 *
 * @author clementval
 */

public class XtypeTable extends Xnode {

  private final Map<String, Xtype> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XtypeTable(Element baseElement) {
    super(baseElement);
    _table = new LinkedHashMap<>();
    readTable();
  }

  /**
   * Read the type table.
   */
  private void readTable() {
    List<Xnode> elements = children();
    for(Xnode n : elements) {
      switch(n.opcode()) {
        case FBASICTYPE:
          XbasicType bt = new XbasicType(n);
          _table.put(bt.getType(), bt);
          break;
        case FFUNCTIONTYPE:
          XfunctionType ft = new XfunctionType(n);
          _table.put(ft.getType(), ft);
          break;
        case FSTRUCTTYPE:
          // TODO create XstructType object and insert it in the table
          break;
      }
    }
  }

  /**
   * Get number of elements in the type table.
   *
   * @return Number of elements in the table.
   */
  public int count() {
    return _table.size();
  }

  /**
   * Add a new element in the type table.
   *
   * @param type The new type to be added.
   */
  public void add(Xtype type) {
    if(!type.getType().isEmpty()) {
      _baseElement.appendChild(type.cloneRawNode());
      _table.put(type.getType(), type);
    }
  }

  /**
   * Get type associated with node if any.
   *
   * @param node Node to retrieve the type.
   * @return Xtype element if found. Null otherwise.
   */
  public Xtype get(Xnode node) {
    return node == null ? null : get(node.getType());
  }

  /**
   * Get an element from the type table.
   *
   * @param type Type of the element to be returned.
   * @return Xtype object if found in the table. Null otherwise.
   */
  public Xtype get(String type) {
    if(_table.containsKey(type)) {
      return _table.get(type);
    }
    return null;
  }

  /**
   * Check if a type is present in the type table
   *
   * @param type Type of the element to be checked.
   * @return True if the element is present. False otherwise.
   */
  public boolean hasType(String type) {
    return _table.containsKey(type);
  }

  /**
   * Generate a unique hash in the current type table.
   *
   * @param type Type to generate the hash.
   * @return New unique hash.
   */
  public String generateHash(XcodeType type) {
    String hash;
    do {
      hash = type.generateHash();
    } while(hasType(hash));
    return hash;
  }

  /**
   * Returns a Collection view of the values contained in this XtypeTable.
   *
   * @return A view of the values contained in this map
   */
  public Collection<Xtype> values() {
    return _table.values();
  }

  @Override
  public XtypeTable cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new XtypeTable(clone);
  }
}
