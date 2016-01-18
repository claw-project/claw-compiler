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
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 *
 * Elements:
 * - Optional:
 *   - varDecl (XvarDecl)
 *   - FstructDecl TODO
 *   - externDecl TODO
 *   - FuseDecl TODO
 *   - FuseOnlyDecl TODO
 *   - FinterfaceDecl TODO
 *   - FnamelistDecl TODO
 *   - FequivalenceDecl TODO
 *   - FcommonDecl TODO
 *
 * @author clementval
 */

public class XdeclTable extends XbaseElement {

  private Hashtable<String, XvarDecl> _table;

  /**
   * Xelement standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root element of the Xelement
   */
  public XdeclTable(Element baseElement){
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the declaration table.
   */
  private void readTable(){
    // Read all varDecl elements
    NodeList nodeList = baseElement
      .getElementsByTagName(XelementName.VAR_DECL);
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node n = nodeList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element)n;
        XvarDecl decl = new XvarDecl(el);
        _table.put(decl.getName().getValue(), decl);
      }
    }

    // TODO read FstructDecl elements
    // TODO read externDecl elements
    // TODO read FuseDecl elements
    // TODO read FuseOnlyDecl elements
    // TODO read FinterfaceDecl elements
    // TODO read FnamelistDecl elements
    // TODO read FequivalenceDecl elements
    // TODO read FcommonDecl elements
  }

  /**
   * Replace a declaration in the table.
   * @param decl The new declaration to be inserted.
   */
  public void replace(XvarDecl decl){
    XvarDecl oldDecl = _table.get(decl.getName().getValue());
    if(oldDecl == null){
      // TODO error handling
    }

    XelementHelper.insertAfter(oldDecl, decl);
    oldDecl.delete();
  }

  /**
   * Add a new declaration.
   * @param decl The new declaration object.
   */
  public void add(XvarDecl decl){
    baseElement.appendChild(decl.clone());
    _table.put(decl.getName().getValue(), decl);
  }

  /**
   * Get a specific declaration based on its name.
   * @param key The name of the declaration to be returned.
   * @return A XvarDecl object if key is found. Null otherwise.
   */
  public XvarDecl get(String key){
    if(_table.containsKey(key)) {
      return _table.get(key);
    }
    return null;
  }

  /**
   * Get the number of declarations in the table.
   * @return The number of declarations in the table.
   */
  public int count(){
    return _table.size();
  }
}
