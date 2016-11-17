/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The XcodeML class represents the basic XcodeML file unit. Both XcodeProgram
 * and Xmod inherit from this class.
 *
 * @author clementval
 */
public class XcodeML extends Xnode {

  private final XtypeTable _typeTable;
  private Document _xcodemlDoc = null;

  /**
   * Constructs a basic XcodeML object representing the XcodeML file given in
   * input.
   *
   * @param baseElement Document representing the XcodeML file.
   */
  public XcodeML(Document baseElement) {
    super(baseElement.getDocumentElement());
    _typeTable = new XtypeTable(matchSeq(Xcode.TYPETABLE).element());
    _xcodemlDoc = baseElement;
  }

  /**
   * @return The XML Document representing the XcodeML file.
   */
  public Document getDocument() {
    return _xcodemlDoc;
  }

  /**
   * Get the type table of the Xmod module.
   *
   * @return The types table.
   */
  public XtypeTable getTypeTable() {
    return _typeTable;
  }

  /**
   * Retrieve all function definitions in the XcodeProgram unit.
   *
   * @return A list of all function definitions in the XcodeProgram unit.
   */
  public List<XfunctionDefinition> getAllFctDef()
  {
    NodeList stmtList =
        getDocument().getElementsByTagName(Xname.F_FUNCTION_DEFINITION);
    List<XfunctionDefinition> definitions = new ArrayList<>();
    for(int i = 0; i < stmtList.getLength(); i++) {
      Node stmtNode = stmtList.item(i);
      if(stmtNode.getNodeType() == Node.ELEMENT_NODE) {
        definitions.add(new XfunctionDefinition((Element) stmtNode));
      }
    }
    return definitions;
  }

  /**
   * Create a copy of a variable element or an integer constant from a XcodeML
   * unit to this unit.
   *
   * @param base       Base element to be copied.
   * @param xcodemlSrc Source XcodeML unit.
   * @return The newly created element in the current XcodeML unit.
   * @throws IllegalTransformationException If the variable element doesn't meet
   *                                        the criteria.
   */
  public Xnode importConstOrVar(Xnode base, XcodeML xcodemlSrc)
      throws IllegalTransformationException
  {
    if(base.opcode() != Xcode.FINTCONSTANT && base.opcode() != Xcode.VAR) {
      throw new IllegalTransformationException(
          String.format("Lower/upper bound type currently not supported (%s)",
              base.opcode().toString())
      );
    }

    if(base.opcode() == Xcode.VAR) {
      return importVar(base, xcodemlSrc);
    } else {
      Xnode intConst = new Xnode(Xcode.FINTCONSTANT, this);
      intConst.setValue(base.value());
      return intConst;
    }
  }

  /**
   * Create a copy with a new hash type of an integer variable element from one
   * XcodeML unit to the current unit.
   *
   * @param base       Base variable element to be copied.
   * @param xcodemlSrc Source XcodeML unit.
   * @return The newly created element in the current XcodeML unit.
   * @throws IllegalTransformationException If the variable element doesn't meet
   *                                        the criteria.
   */
  private Xnode importVar(Xnode base, XcodeML xcodemlSrc)
      throws IllegalTransformationException
  {
    String typeValue = base.getAttribute(Xattr.TYPE);
    if(!typeValue.startsWith(Xtype.PREFIX_INTEGER)) {
      throw new IllegalTransformationException("Only integer variable are " +
          "supported as lower/upper bound value for promotted arrays.");
    }

    XbasicType type = (XbasicType) xcodemlSrc.getTypeTable().get(typeValue);
    Xnode bType = new Xnode(Xcode.FBASICTYPE, this);
    bType.setAttribute(Xattr.REF, Xname.TYPE_F_INT);
    bType.setAttribute(Xattr.TYPE, getTypeTable().generateIntegerTypeHash());
    if(type != null && type.getIntent() != Xintent.NONE) {
      bType.setAttribute(Xattr.INTENT, type.getIntent().toString());
    }

    Xnode var = new Xnode(Xcode.VAR, this);
    var.setAttribute(Xattr.SCOPE, base.getAttribute(Xattr.SCOPE));
    var.setValue(base.value());
    var.setAttribute(Xattr.TYPE, bType.getAttribute(Xattr.TYPE));
    return var;
  }

  /**
   * Import a type description from one XcodeML unit to the current one. If the
   * type id is not present in the source XcodeML unit, nothing is done.
   *
   * @param src    Source XcodeML unit.
   * @param typeId Type id to be imported.
   */
  public void importType(XcodeML src, String typeId) {
    if(typeId == null || getTypeTable().hasType(typeId)) {
      return;
    }
    Xtype type = src.getTypeTable().get(typeId);
    if(type == null) {
      return;
    }
    Node rawNode = getDocument().importNode(type.element(), true);
    Xtype importedType = new Xtype((Element) rawNode);
    getTypeTable().add(importedType);
    if(importedType.hasAttribute(Xattr.REF)
        && !XnodeUtil.isBuiltInType(importedType.getAttribute(Xattr.REF)))
    {
      importType(src, importedType.getAttribute(Xattr.REF));
    }

    // Handle possible type ref in indexRange element
    List<Xnode> vars = importedType.matchAll(Xcode.VAR);
    for(Xnode var : vars) {
      importType(src, var.getAttribute(Xattr.TYPE));
    }
  }

  /**
   * Write the XcodeML to file or std out
   *
   * @param outputFile Path of the output file or null to output on std out
   * @param indent     Number of spaces used for the indentation
   * @throws IllegalTransformationException if XML file cannot be written.
   */
  public void write(String outputFile, int indent)
      throws IllegalTransformationException
  {
    try {
      cleanEmptyTextNodes(this.getDocument());
      Transformer transformer
          = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty(
          "{http://xml.apache.org/xslt}indent-amount",
          Integer.toString(indent));
      DOMSource source = new DOMSource(this.getDocument());
      if(outputFile == null) {
        // Output to console
        StreamResult console = new StreamResult(System.out);
        transformer.transform(source, console);
      } else {
        // Output to file
        StreamResult console = new StreamResult(new File(outputFile));
        transformer.transform(source, console);
      }
    } catch(Exception ignored) {
      throw new IllegalTransformationException("Cannot output file: " +
          outputFile, 0);
    }
  }

  /**
   * Removes text nodes that only contains whitespace. The conditions for
   * removing text nodes, besides only containing whitespace, are: If the
   * parent node has at least one child of any of the following types, all
   * whitespace-only text-node children will be removed: - ELEMENT child -
   * CDATA child - COMMENT child.
   *
   * @param parentNode Root node to start the cleaning.
   */
  private void cleanEmptyTextNodes(Node parentNode) {
    boolean removeEmptyTextNodes = false;
    Node childNode = parentNode.getFirstChild();
    while(childNode != null) {
      removeEmptyTextNodes |= checkNodeTypes(childNode);
      childNode = childNode.getNextSibling();
    }

    if(removeEmptyTextNodes) {
      removeEmptyTextNodes(parentNode);
    }
  }

  /**
   * Remove all empty text nodes in the subtree.
   *
   * @param parentNode Root node to start the search.
   */
  private void removeEmptyTextNodes(Node parentNode) {
    Node childNode = parentNode.getFirstChild();
    while(childNode != null) {
      // grab the "nextSibling" before the child node is removed
      Node nextChild = childNode.getNextSibling();
      short nodeType = childNode.getNodeType();
      if(nodeType == Node.TEXT_NODE) {
        boolean containsOnlyWhitespace = childNode.getNodeValue()
            .trim().isEmpty();
        if(containsOnlyWhitespace) {
          parentNode.removeChild(childNode);
        }
      }
      childNode = nextChild;
    }
  }

  /**
   * Check the type of the given node.
   *
   * @param childNode Node to be checked.
   * @return True if the node contains data. False otherwise.
   */
  private boolean checkNodeTypes(Node childNode) {
    short nodeType = childNode.getNodeType();
    if(nodeType == Node.ELEMENT_NODE) {
      cleanEmptyTextNodes(childNode); // recurse into subtree
    }
    return nodeType == Node.ELEMENT_NODE || nodeType == Node.CDATA_SECTION_NODE
        || nodeType == Node.COMMENT_NODE;
  }


  /*
   * Node creation section
   */

  /**
   * Constructs a new name node with name value and optional type.
   *
   * @param name Name value.
   * @param type Optional type value.
   * @return The newly created node dettached in the current XcodeML unit.
   */
  public Xnode createName(String name, String type)
  {
    Xnode n = new Xnode(Xcode.NAME, this);
    n.setValue(name);
    if(type != null) {
      n.setAttribute(Xattr.TYPE, type);
    }
    return n;
  }

  /**
   * Create the id and varDecl nodes and add them to the symbol/declaration
   * table.
   *
   * @param name   Name of the variable.
   * @param type   Type of the variable.
   * @param sclass Scope class of the variable (from Xname).
   * @param fctDef Function definition in which id and decl are created.
   */
  public void createIdAndDecl(String name, String type, String sclass,
                              XfunctionDefinition fctDef)
  {
    Xid id = XnodeUtil.createId(this, type, sclass, name);
    fctDef.getSymbolTable().add(id);
    Xdecl decl = XnodeUtil.createVarDecl(this, type, name);
    fctDef.getDeclarationTable().add(decl);
  }

  /**
   * Create a new namedValue node with its attribute.
   *
   * @param value Value of the name attribute.
   * @return The newly created node dettached in the current XcodeML unit.
   */
  public Xnode createNamedValue(String value) {
    Xnode namedValue = new Xnode(Xcode.NAMEDVALUE, this);
    namedValue.setAttribute(Xattr.NAME, value);
    return namedValue;
  }

  /**
   * Create a new var node.
   * <p>
   * {@code
   * <Var type="" scope="">value</Var>
   * }
   *
   * @param type  Value of the type attribute.
   * @param value Value of the var.
   * @param scope Value of the scope attribute.
   * @return The newly created node dettached in the current XcodeML unit.
   */
  public Xnode createVar(String type, String value, Xscope scope) {
    Xnode var = new Xnode(Xcode.VAR, this);
    var.setAttribute(Xattr.TYPE, type);
    var.setAttribute(Xattr.SCOPE, scope.toString());
    var.setValue(value);
    return var;
  }

  /**
   * Create a new FunctionCall node with name and arguments as children nodes.
   *
   * @param returnType Value of the type attribute for the functionCall node.
   * @param name       Value of the name node.
   * @param nameType   Value of the type attribute for the name node.
   * @return The newly created node dettached in the current XcodeML unit.
   */
  public Xnode createFctCall(String returnType, String name, String nameType) {
    Xnode fctCall = new Xnode(Xcode.FUNCTIONCALL, this);
    fctCall.setAttribute(Xattr.TYPE, returnType);
    Xnode fctName = new Xnode(Xcode.NAME, this);
    fctName.setValue(name);
    fctName.setAttribute(Xattr.TYPE, nameType);
    Xnode args = new Xnode(Xcode.ARGUMENTS, this);
    fctCall.append(fctName, false);
    fctCall.append(args, false);
    return fctCall;
  }
}
