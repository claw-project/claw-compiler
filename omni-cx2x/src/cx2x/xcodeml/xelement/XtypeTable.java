/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import java.util.Hashtable;
import java.util.Random;

/**
 * The XtypeTable represents the typeTable (3.1) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - FbasicType (XbasicType)
 *   - FfunctionType (XfctType)
 *   - FstructType (XstructType)
 *
 * @author clementval
 */

public class XtypeTable extends XbaseElement {

  private static final int HASH_LENGTH = 12;
  private static final String FCT_HASH_PREFIX = "F";

  private Hashtable<String, Xtype> _table;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XtypeTable(Element baseElement){
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the type table.
   */
  private void readTable(){
    // Read basic type
    NodeList basicTypes = baseElement.getElementsByTagName(XelementName.BASIC_TYPE);
    for (int i = 0; i < basicTypes.getLength(); i++) {
      Node n = basicTypes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XbasicType bt = new XbasicType(el);
        _table.put(bt.getType(), bt);
      }
    }

    // Read fct type
    NodeList fctTypes = baseElement.getElementsByTagName(XelementName.FCT_TYPE);
    for (int i = 0; i < fctTypes.getLength(); i++) {
      Node n = fctTypes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XfctType ft = new XfctType(el);
        _table.put(ft.getType(), ft);
      }
    }

    // Read struct type
    NodeList structTypes =
      baseElement.getElementsByTagName(XelementName.F_STRUCT_TYPE);
    for (int i = 0; i < structTypes.getLength(); i++) {
      Node n = structTypes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        // TODO create XstructType object and insert it in the table
      }
    }
  }

  /**
   * Get number of elements in the type table.
   * @return Number of elements in the table.
   */
  public int count(){
    return _table.size();
  }

  /**
   * Add a new element in the type table.
   * @param type The new type to be added.
   */
  public void add(Xtype type){
    baseElement.appendChild(type.cloneNode());
    _table.put(type.getType(), type);
  }

  /**
   * Get an element from the type table.
   * @param key Key of the element to be returned.
   * @return Xtype object if found in the table. Null otherwise.
   */
  public Xtype get(String key) {
    if(_table.containsKey(key)){
      return _table.get(key);
    }
    return null;
  }

  /**
   * Get a new unique function hash for the type table.
   * @return A unique function hash as String value.
   */
  public String generateFctTypeHash(){
    String hash;
    do {
      hash = FCT_HASH_PREFIX + generateHash(HASH_LENGTH);
    } while(_table.containsKey(hash));
    return hash;
  }

  /**
   * Generate a new unique type hash for the table.
   * @param length Length of the hash string to be generated.
   * @return The new unique hash.
   */
  private String generateHash(int length){
    Random r = new Random();
    StringBuilder sb = new StringBuilder();
    while(sb.length() < length){
      sb.append(Integer.toHexString(r.nextInt()));
    }
    return sb.toString().substring(0, length);
  }
}
