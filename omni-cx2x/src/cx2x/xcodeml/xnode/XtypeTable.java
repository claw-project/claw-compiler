/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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
   * @param node Raw node.
   */
  public XtypeTable(Xnode node) {
    super(node == null ? null : node.element());
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
          XstructType st = new XstructType(n);
          _table.put(st.getType(), st);
          break;
      }
    }
  }

  /**
   * Check if the node is of type XbasicType.
   *
   * @param node Node to check.
   * @return True if the node is of type XbasicType.
   */
  public boolean isBasicType(Xnode node) {
    return isBasicType(node.getType());
  }

  /**
   * Check if the hash correspond to a XbasicType.
   *
   * @param hash Hash of type to check.
   * @return True if the hash correspond to a XbasicType.
   */
  public boolean isBasicType(String hash) {
    return isType(XbasicType.class, hash);
  }

  /**
   * Check if the node is of type XfunctionType.
   *
   * @param node Node to check.
   * @return True if the node is of type XfunctionType.
   */
  public boolean isFunctionType(Xnode node) {
    return isFunctionType(node.getType());
  }

  /**
   * Check if the hash correspond to a XfunctionType.
   *
   * @param hash Hash of type to check.
   * @return True if the hash correspond to a XfunctionType.
   */
  public boolean isFunctionType(String hash) {
    return isType(XfunctionType.class, hash);
  }

  /**
   * Check if the hash correspond to a XstructType.
   *
   * @param hash Hash of type to check.
   * @return True if the hash correspond to a XstructType.
   */
  public boolean isStructType(String hash) {
    return isType(XstructType.class, hash);
  }

  /**
   * Check if the hash correspond to a XstructType.
   *
   * @param node Node to check.
   * @return True if the hash correspond to a XstructType.
   */
  public boolean isStructType(Xnode node) {
    return isStructType(node.getType());
  }

  /**
   * Check if the corresponding type node is of given type.
   *
   * @param typeClass Class type to check against.
   * @param hash      Hash of type to retrieved.
   * @return True of the type is retrieved and is of given type. False in any
   * other case.
   */
  private boolean isType(Class typeClass, String hash) {
    if(hash == null || hash.isEmpty()) {
      return false;
    }
    Xnode t = get(hash);
    return t != null && typeClass.isInstance(t);
  }

  /**
   * Get number of elements in the type table.
   *
   * @return Number of elements in the table.
   */
  public int size() {
    return _table.size();
  }

  /**
   * Add a new element in the type table.
   *
   * @param type The new type to be added.
   */
  public void add(Xnode type) {
    if(!type.getType().isEmpty()) {
      // TODO should be cloned?
      _baseElement.appendChild(type.cloneRawNode());
      _table.put(type.getType(), type);
    }
  }

  /**
   * Get the XbasicType associated with the node if any.
   *
   * @param node Node to look for type.
   * @return XbasicType if associated. Null otherwise.
   */
  public XbasicType getBasicType(Xnode node) {
    return getBasicType(node.getType());
  }

  /**
   * Get the XbasicType associated with the given hash value.
   *
   * @param hash Hash value to check for.
   * @return XbasicType if associated. Null otherwise.
   */
  public XbasicType getBasicType(String hash) {
    if(isBasicType(hash)) {
      return (XbasicType) get(hash);
    }
    return null;
  }

  /**
   * Get the XfunctionType associated with the node if any.
   *
   * @param node Node to look for type.
   * @return XfunctionType if associated. Null otherwise.
   */
  public XfunctionType getFunctionType(Xnode node) {
    return getFunctionType(node.getType());
  }

  /**
   * Get the XfunctionType associated with the given hash value.
   *
   * @param hash Hash value to check for.
   * @return XfunctionType if associated. Null otherwise.
   */
  public XfunctionType getFunctionType(String hash) {
    if(isFunctionType(hash)) {
      return (XfunctionType) get(hash);
    }
    return null;
  }

  /**
   * Get the XstructType associated with the node if any.
   *
   * @param node Node to look for type.
   * @return XstructType if associated. Null otherwise.
   */
  public XstructType getStructType(Xnode node) {
    return getStructType(node.getType());
  }

  /**
   * Get the XstructType associated with the given hash value.
   *
   * @param hash Hash value to check for.
   * @return XstructType if associated. Null otherwise.
   */
  public XstructType getStructType(String hash) {
    if(isStructType(hash)) {
      return (XstructType) get(hash);
    }
    return null;
  }

  /**
   * Get an element from the type table.
   *
   * @param hash Hash of type node to be retrieved.
   * @return Xnode object if found in the table. Null otherwise.
   */
  protected Xnode get(String hash) {
    if(_table.containsKey(hash)) {
      return _table.get(hash);
    }
    return null;
  }

  /**
   * Check if a type is present in the type table
   *
   * @param hash Hash of type node to be checked.
   * @return True if the element is present. False otherwise.
   */
  public boolean hasType(String hash) {
    return _table.containsKey(hash);
  }

  /**
   * Generate a unique hash in the current type table.
   *
   * @param type Type to generate the hash.
   * @return New unique hash.
   */
  public String generateHash(XcodeType type) {
    if(type == null) {
      return "";
    }
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
    return new XtypeTable(super.cloneNode());
  }
}
