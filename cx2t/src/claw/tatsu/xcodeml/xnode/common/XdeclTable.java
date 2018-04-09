/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import java.util.*;

/**
 * The XdeclTable represents the typeTable (5.2) element in XcodeML intermediate
 * representation.
 *
 * Elements: ( varDecl | FstructDecl | externDecl | FuseDecl | FuseOnlyDecl
 * | FinterfaceDecl | FnamelistDecl | FequivalenceDecl
 * | FcommonDecl )*
 *
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
  private final LinkedHashMap<String, Xnode> _table;

  /**
   * Element standard ctor. Pass the base element to the base class and read
   * inner information (elements and attributes).
   *
   * @param node Raw node.
   */
  public XdeclTable(Xnode node) {
    super(node == null ? null : node.element());
    _table = new LinkedHashMap<>();
    readTable();
  }

  /**
   * Read the declaration table.
   */
  private void readTable() {
    List<Xnode> declarations = children();
    for(Xnode n : declarations) {
      String key = "";
      switch(n.opcode()) {
        case EXTERN_DECL:
        case F_STRUCT_DECL:
        case VAR_DECL:
          key = n.matchSeq(Xcode.NAME).value();
          break;
        case F_USE_DECL:
        case F_USE_ONLY_DECL:
        case F_INTERFACE_DECL:
          key = n.getAttribute(Xattr.NAME);
          break;
        case F_NAMELIST_DECL:
          key = n.matchSeq(Xcode.VAR_LIST).getAttribute(Xattr.NAME);
          break;
        case F_COMMON_DECL:
          key = Xcode.F_COMMON_DECL.toString() + UUID.randomUUID();
          break;
        case F_EQUIVALENCE_DECL:
          key = Xcode.F_EQUIVALENCE_DECL.toString() + UUID.randomUUID();
          break;
      }
      _table.put(key, n);
    }

  }

  /**
   * Replace a declaration in the table.
   *
   * @param decl The new declaration to be inserted.
   * @param name Name describing the declaration in the table.
   */
  public void replace(Xnode decl, String name) {
    Xnode oldDecl = _table.get(name);
    if(oldDecl == null) {
      append(decl);
    } else {
      oldDecl.insertAfter(decl);
      oldDecl.delete();
    }
  }

  /**
   * Add a new declaration as last element.
   *
   * @param decl The new declaration object.
   */
  public void add(Xnode decl) {
    _baseElement.appendChild(decl.cloneRawNode());
    _table.put(decl.matchSeq(Xcode.NAME).value(), decl);
  }

  /**
   * Add a new declaration as last element.
   *
   * @param decl The new declaration object.
   */
  public void addFirst(Xnode decl) {
    if(_baseElement.getFirstChild() != null) {
      _baseElement.insertBefore(decl.cloneRawNode(),
          _baseElement.getFirstChild());
    } else {
      _baseElement.appendChild(decl.cloneRawNode());
    }
    _table.put(decl.matchSeq(Xcode.NAME).value(), decl);
  }

  /**
   * Get a specific declaration based on its name.
   *
   * @param key The name of the declaration to be returned.
   * @return A XvarDecl object if key is found. Null otherwise.
   */
  public Xnode get(String key) {
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
  public List<Xnode> values() {
    List<Xnode> orderedDeclarations = new ArrayList<>();
    for(Map.Entry<String, Xnode> entry : _table.entrySet()) {
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
  public List<Xnode> values(Xcode decl) {
    return values(Collections.singletonList(decl));
  }

  /**
   * Get all declarations of a specific kind of elements.
   *
   * @param declarations Kind of elements to return.
   * @return A list of all declarations of this kind.
   */
  public List<Xnode> values(List<Xcode> declarations) {
    List<Xnode> orderedFilteredDeclarations = new ArrayList<>();
    for(Map.Entry<String, Xnode> entry : _table.entrySet()) {
      if(declarations.contains(entry.getValue().opcode())) {
        orderedFilteredDeclarations.add(entry.getValue());
      }
    }
    return orderedFilteredDeclarations;
  }

  /**
   * Get all the use and use only declaration in the table.
   *
   * @return List of FuseDecl and FuseOnlyDecl nodes.
   */
  public List<Xnode> uses() {
    return values(Arrays.asList(Xcode.F_USE_DECL, Xcode.F_USE_ONLY_DECL));
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
  public void checkOrder(FfunctionDefinition fct) {
    int functionLineNo = fct.lineNo();
    List<Xnode> decl = new ArrayList<>();

    // TODO DOM element
    Node crtNode = _baseElement.getFirstChild();
    while(crtNode != null) {
      if(crtNode.getNodeType() == Node.ELEMENT_NODE) {
        Xnode node = new Xnode((Element) crtNode);
        // Only var declarations can be disordered
        if(node.opcode() == Xcode.VAR_DECL
            || node.opcode() == Xcode.F_STRUCT_DECL)
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

  @Override
  public XdeclTable cloneNode() {
    return new XdeclTable(super.cloneNode());
  }
}
