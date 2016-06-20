/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.Collection;
import java.util.Hashtable;

import cx2x.xcodeml.helper.*;

/**
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( varDecl | FstructDecl | externDecl | FuseDecl | FuseOnlyDecl
 *            | FinterfaceDecl | FnamelistDecl | FequivalenceDecl
 *            | FcommonDecl )*
 *
 * - Optional:
 *   - varDecl (XvarDecl)
 *   - FstructDecl
 *   - externDecl
 *   - FuseDecl
 *   - FuseOnlyDecl
 *   - FinterfaceDecl
 *   - FnamelistDecl
 *   - FequivalenceDecl
 *   - FcommonDecl
 *
 * @author clementval
 */

public class XdeclTable extends Xnode {

  // TODO move to Xdecl
  private final Hashtable<String, XvarDecl> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   * @param baseElement The root of the element.
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
    Node crtNode = _baseElement.getFirstChild();
    while(crtNode != null){
      if (crtNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element)crtNode;

        switch (element.getTagName()){
          case Xname.VAR_DECL:
            XvarDecl decl = new XvarDecl(element);
            _table.put(decl.getName().getValue(), decl);
            break;

          // TODO read FstructDecl elements
          // TODO read externDecl elements
          // TODO read FuseDecl elements
          // TODO read FuseOnlyDecl elements
          // TODO read FinterfaceDecl elements
          // TODO read FnamelistDecl elements
          // TODO read FequivalenceDecl elements
          // TODO read FcommonDecl elements

        }
      }
      crtNode = crtNode.getNextSibling();
    }
  }

  /**
   * Replace a declaration in the table.
   * @param decl The new declaration to be inserted.
   */
  public void replace(XvarDecl decl){
    XvarDecl oldDecl = _table.get(decl.getName().getValue());
    if(oldDecl == null){
      appendToChildren(decl, false);
    } else {
      XnodeUtil.insertAfter(oldDecl, decl);
      oldDecl.delete();
    }
  }

  /**
   * Add a new declaration.
   * @param decl The new declaration object.
   */
  public void add(XvarDecl decl){
    _baseElement.appendChild(decl.cloneNode());
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
   * Get all elements in the table.
   * @return All elements stored in the table.
   * TODO move to Xdecl
   */
  public Collection<XvarDecl> getAll(){
    return _table.values();
  }

  /**
   * Get the number of declarations in the table.
   * @return The number of declarations in the table.
   */
  public int count(){
    return _table.size();
  }

  /**
   * Check if a name is already present in the declaration table.
   * @param name String value of the name to check.
   * @return True if the name is already in the table. False otherwise.
   */
  public boolean contains(String name){
    return _table.containsKey(name);
  }


  @Override
  public XdeclTable cloneObject() {
    Element clone = (Element)cloneNode();
    return new XdeclTable(clone);
  }
}
