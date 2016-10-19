/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.helper.XnodeUtil;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.*;

/**
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 * <p>
 * Elements: ( varDecl | FstructDecl | externDecl | FuseDecl | FuseOnlyDecl
 * | FinterfaceDecl | FnamelistDecl | FequivalenceDecl
 * | FcommonDecl )*
 * <p>
 * - Optional:
 * - varDecl
 * - FstructDecl
 * - externDecl
 * - FuseDecl
 * - FuseOnlyDecl
 * - FinterfaceDecl
 * - FnamelistDecl
 * - FequivalenceDecl
 * - FcommonDecl
 *
 * @author clementval
 */

public class XdeclTable extends Xnode {

  private final Hashtable<String, Xdecl> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XdeclTable(Element baseElement) {
    super(baseElement);
    _table = new Hashtable<>();
    readTable();
  }

  /**
   * Read the declaration table.
   */
  private void readTable() {
    Node crtNode = _baseElement.getFirstChild();
    while(crtNode != null) {
      if(crtNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) crtNode;

        switch(element.getTagName()) {
          case Xname.VAR_DECL:
            Xdecl varDecl = new Xdecl(element);
            _table.put(varDecl.find(Xcode.NAME).getValue(), varDecl);
            break;
          case Xname.F_USE_DECL:
          case Xname.F_USE_ONLY_DECL:
            Xdecl useDecl = new Xdecl(element);
            _table.put(useDecl.getAttribute(Xattr.NAME), useDecl);
            break;


          // TODO read FstructDecl elements
          // TODO read externDecl elements
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
   *
   * @param decl The new declaration to be inserted.
   * @param name Name describing the declaration in the table.
   */
  public void replace(Xdecl decl, String name) {
    Xdecl oldDecl = _table.get(name);
    if(oldDecl == null) {
      appendToChildren(decl, false);
    } else {
      XnodeUtil.insertAfter(oldDecl, decl);
      oldDecl.delete();
    }
  }

  /**
   * Add a new declaration.
   *
   * @param decl The new declaration object.
   */
  public void add(Xdecl decl) {
    _baseElement.appendChild(decl.cloneNode());
    _table.put(decl.find(Xcode.NAME).getValue(), decl);
  }

  /**
   * Get a specific declaration based on its name.
   *
   * @param key The name of the declaration to be returned.
   * @return A XvarDecl object if key is found. Null otherwise.
   */
  public Xdecl get(String key) {
    if(_table.containsKey(key)) {
      return _table.get(key);
    }
    return null;
  }

  /**
   * Get all elements in the table.
   *
   * @return All elements stored in the table.
   */
  public Collection<Xdecl> getAll() {
    return _table.values();
  }

  /**
   * Get all declarations of a specific kind of elements.
   *
   * @param decl Kind of elements to return.
   * @return A list of all declarations of this kind.
   */
  public List<Xdecl> getAll(Xcode decl) {

    switch(decl) {
      case VARDECL:
      case FUSEDECL:
      case FUSEONLYDECL:
      case FSTRUCTDECL:
      case EXTERNDECL:
      case FINTERFACEDECL:
      case FNAMELISTDECL:
      case FEQUIVALENCEDECL:
      case FCOMMONDECL:
        Iterator<Map.Entry<String, Xdecl>> it = _table.entrySet().iterator();
        List<Xdecl> decls = new ArrayList<>();
        while(it.hasNext()) {
          Map.Entry<String, Xdecl> entry = it.next();
          if(entry.getValue().opcode() == decl) {
            decls.add(entry.getValue());
          }
        }
        return decls;
      default:
        throw new IllegalArgumentException("Not a member of the decl table");
    }
  }

  /**
   * Get the number of declarations in the table.
   *
   * @return The number of declarations in the table.
   */
  public int count() {
    return _table.size();
  }

  /**
   * Check if a name is already present in the declaration table.
   *
   * @param name String value of the name to check.
   * @return True if the name is already in the table. False otherwise.
   */
  public boolean contains(String name) {
    return _table.containsKey(name);
  }

  /**
   * Check if the order of declaration makes sense. If not, fix it.
   *
   * @param fct Function definition which is checked.
   */
  public void checkOrder(XfunctionDefinition fct) {
    int functionLineNo = fct.getLineNo();
    List<Xnode> decl = new ArrayList<>();

    Node crtNode = _baseElement.getFirstChild();
    while(crtNode != null) {
      if(crtNode.getNodeType() == Node.ELEMENT_NODE) {
        Xnode node = new Xnode((Element) crtNode);
        // Only var declarations can be disordered
        if(node.opcode() == Xcode.VARDECL
            || node.opcode() == Xcode.FSTRUCTDECL)
        {
          decl.add(node);
        }
      }
      crtNode = crtNode.getNextSibling();
    }

    if(decl.size() < 2) {
      return;
    }

    int firstDeclLineNo = decl.get(0).getLineNo();
    int secondDeclLineNo = decl.get(1).getLineNo();

    if(functionLineNo == firstDeclLineNo) {
      _baseElement.appendChild(decl.get(0).getElement());
    } else if(firstDeclLineNo > secondDeclLineNo) {
      Xnode hook = decl.get(1);
      for(int i = 1; i < decl.size(); ++i) {
        if(decl.get(i).getLineNo() > firstDeclLineNo) {
          break;
        }
        hook = decl.get(i);
      }
      XnodeUtil.insertAfter(hook, decl.get(0));
    }
  }

  @Override
  public XdeclTable cloneObject() {
    Element clone = (Element) cloneNode();
    return new XdeclTable(clone);
  }
}
