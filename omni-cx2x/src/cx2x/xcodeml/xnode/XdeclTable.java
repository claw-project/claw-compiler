/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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

  /* Some transformation needs to know the order of the declarations. Therefore,
   * we use a LinkedHashMap to be able to give back the table with its order. */
  private final LinkedHashMap<String, Xdecl> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param baseElement The root of the element.
   */
  public XdeclTable(Element baseElement) {
    super(baseElement);
    _table = new LinkedHashMap<>();
    readTable();
  }

  /**
   * Read the declaration table.
   */
  private void readTable() {
    List<Xnode> declarations = children();
    for(Xnode n : declarations) {
      Xdecl decl = new Xdecl(n);
      String key = "";
      switch(n.opcode()) {
        case EXTERNDECL:
        case FSTRUCTDECL:
        case VARDECL:
          key = decl.matchSeq(Xcode.NAME).value();
          break;
        case FUSEDECL:
        case FUSEONLYDECL:
        case FINTERFACEDECL:
          key = decl.getAttribute(Xattr.NAME);
          break;
        case FNAMELISTDECL:
          key = decl.matchSeq(Xcode.VARLIST).getAttribute(Xattr.NAME);
          break;
        case FCOMMONDECL:
          key = Xcode.FCOMMONDECL.toString() + UUID.randomUUID();
          break;
        case FEQUIVALENCEDECL:
          key = Xcode.FEQUIVALENCEDECL.toString() + UUID.randomUUID();
          break;
      }
      _table.put(key, decl);
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
      append(decl, false);
    } else {
      oldDecl.insertAfter(decl);
      oldDecl.delete();
    }
  }

  /**
   * Add a new declaration.
   *
   * @param decl The new declaration object.
   */
  public void add(Xdecl decl) {
    _baseElement.appendChild(decl.cloneRawNode());
    _table.put(decl.matchSeq(Xcode.NAME).value(), decl);
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
  public List<Xdecl> values() {
    List<Xdecl> orderedDeclarations = new ArrayList<>();
    for(Map.Entry<String, Xdecl> entry : _table.entrySet()) {
      orderedDeclarations.add(entry.getValue());
    }
    return orderedDeclarations;
  }

  /**
   * Get all declarations of a specific kind of elements.
   *
   * @param decl Kind of elements to return.
   * @return A list of all declarations of this kind.
   */
  public List<Xdecl> values(Xcode decl) {
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
        List<Xdecl> orderedFilteredDeclarations = new ArrayList<>();
        for(Map.Entry<String, Xdecl> entry : _table.entrySet()) {
          if(entry.getValue().opcode() == decl) {
            orderedFilteredDeclarations.add(entry.getValue());
          }
        }
        return orderedFilteredDeclarations;
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
    int functionLineNo = fct.lineNo();
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

    int firstDeclLineNo = decl.get(0).lineNo();
    int secondDeclLineNo = decl.get(1).lineNo();

    if(functionLineNo == firstDeclLineNo) {
      _baseElement.appendChild(decl.get(0).element());
    } else if(firstDeclLineNo > secondDeclLineNo) {
      Xnode hook = decl.get(1);
      for(int i = 1; i < decl.size(); ++i) {
        if(decl.get(i).lineNo() > firstDeclLineNo) {
          break;
        }
        hook = decl.get(i);
      }
      hook.insertAfter(decl.get(0));
    }
  }

  /**
   * Get all the USE statement declaration in the table.
   *
   * @return A list of all declaration. Empty list if no USE declaration.
   */
  public List<Xdecl> getAllUseStmts() {
    List<Xdecl> uses = this.values(Xcode.FUSEDECL);
    uses.addAll(this.values(Xcode.FUSEONLYDECL));
    return uses;
  }

  @Override
  public XdeclTable cloneNode() {
    Element clone = (Element) cloneRawNode();
    return new XdeclTable(clone);
  }
}
