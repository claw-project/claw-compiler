/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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

  private final Map<String, Xnode> _table;

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

  public boolean isBasicType(Xnode node) {
    return isBasicType(node.getType());
  }

  public boolean isBasicType(String hash) {
    return isType(XbasicType.class, hash);
  }

  public boolean isFunctionType(Xnode node) {
    return isFunctionType(node.getType());
  }

  public boolean isFunctionType(String hash) {
    return isType(XfunctionType.class, hash);
  }

  public boolean isStructType(String hash) {
    return false; // TODO
  }

  private boolean isType(Class typeClass, String type) {
    if(type == null || type.isEmpty()) {
      return false;
    }
    Xnode t = get(type);
    return t != null && typeClass.isInstance(t);
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
  public void add(Xnode type) {
    if(!type.getType().isEmpty()) {
      _baseElement.appendChild(type.cloneRawNode());
      _table.put(type.getType(), type);
    }
  }

  public XbasicType getBasicType(Xnode node) {
    return getBasicType(node.getType());
  }

  public XbasicType getBasicType(String hash) {
    if(isBasicType(hash)) {
      return (XbasicType) get(hash);
    }
    return null;
  }

  public XfunctionType getFunctionType(Xnode node) {
    return getFunctionType(node.getType());
  }

  public XfunctionType getFunctionType(String hash) {
    if(isFunctionType(hash)) {
      return (XfunctionType) get(hash);
    }
    return null;
  }

  /**
   * Get an element from the type table.
   *
   * @param type Type of the element to be returned.
   * @return Xnode object if found in the table. Null otherwise.
   */
  protected Xnode get(String type) {
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
  public Collection<Xnode> values() {
    return _table.values();
  }

  @Override
  public XtypeTable cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new XtypeTable(clone);
  }
}
