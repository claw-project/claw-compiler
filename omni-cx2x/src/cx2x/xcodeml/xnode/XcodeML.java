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
  XcodeML(Document baseElement) {
    super(baseElement.getDocumentElement());
    _typeTable = new XtypeTable(matchSeq(Xcode.TYPETABLE).element());
    _xcodemlDoc = baseElement;
  }

  /**
   * Create a new node in the current translation unit.
   *
   * @param opcode Opcode of the new node.
   * @return Newly created node.
   */
  public Xnode createNode(Xcode opcode) {
    return new Xnode(opcode, this);
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
      Xnode intConst = createNode(Xcode.FINTCONSTANT);
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
    String typeValue = base.getType();
    if(!typeValue.startsWith(Xtype.PREFIX_INTEGER)) {
      throw new IllegalTransformationException("Only integer variable are " +
          "supported as lower/upper bound value for promoted arrays.");
    }

    XbasicType type = (XbasicType) xcodemlSrc.getTypeTable().get(typeValue);
    Xnode bType = createNode(Xcode.FBASICTYPE);
    bType.setAttribute(Xattr.REF, Xname.TYPE_F_INT);
    bType.setAttribute(Xattr.TYPE, getTypeTable().generateIntegerTypeHash());
    if(type != null && type.getIntent() != Xintent.NONE) {
      bType.setAttribute(Xattr.INTENT, type.getIntent().toString());
    }

    Xnode var = createNode(Xcode.VAR);
    var.setAttribute(Xattr.SCOPE, base.getAttribute(Xattr.SCOPE));
    var.setValue(base.value());
    var.setAttribute(Xattr.TYPE, bType.getType());
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
      importType(src, var.getType());
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
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createName(String name, String type)
  {
    Xnode n = createNode(Xcode.NAME);
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
   * @param name           Name of the variable.
   * @param type           Type of the variable.
   * @param sclass         Scope class of the variable (from Xname).
   * @param fctDef         Function definition in which id and decl are created.
   * @param afterDummyArgs If true, the new variable is declared just after the
   *                       dummy argument. If false, the variable is append at
   *                       the end.
   */
  public void createIdAndDecl(String name, String type, String sclass,
                              XfunctionDefinition fctDef,
                              boolean afterDummyArgs)
  {
    Xid id = createId(type, sclass, name);
    fctDef.getSymbolTable().add(id);
    Xnode decl = createVarDecl(type, name);
    Xnode hook = null;

    // Check where is the last dummy arguments in the declaration
    if(afterDummyArgs) {
      XfunctionType fctType = (XfunctionType) getTypeTable().get(fctDef);
      List<String> parameters = fctType.getParamsNames();

      for(Xnode n : fctDef.getDeclarationTable().values()) {
        if(n.opcode() == Xcode.VARDECL) {
          String varId = n.matchDirectDescendant(Xcode.NAME).value();
          if(n.lineNo() == 0
              || varId.toLowerCase().equals(fctDef.getName().value()))
          {
            continue;
          }
          if(parameters.contains(varId.toLowerCase())) {
            hook = n;
          } else {
            break;
          }
        }
      }
    }

    // Insert the new declaration
    if(hook == null) {
      fctDef.getDeclarationTable().add(decl);
    } else {
      hook.insertAfter(decl);
    }
  }

  /**
   * Create a new namedValue node with its attribute.
   *
   * @param value Value of the name attribute.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createNamedValue(String value) {
    Xnode namedValue = createNode(Xcode.NAMEDVALUE);
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
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVar(String type, String value, Xscope scope) {
    Xnode var = createNode(Xcode.VAR);
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
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createFctCall(String returnType, String name, String nameType) {
    Xnode fctCall = createNode(Xcode.FUNCTIONCALL);
    fctCall.setAttribute(Xattr.TYPE, returnType);
    Xnode fctName = createNode(Xcode.NAME);
    fctName.setValue(name);
    fctName.setAttribute(Xattr.TYPE, nameType);
    Xnode args = createNode(Xcode.ARGUMENTS);
    fctCall.append(fctName);
    fctCall.append(args);
    return fctCall;
  }

  /**
   * Create a new FarrayRef node with varRef node as a child with the
   * given Var element.
   *
   * @param type Value of the type attribute for the FarrayRef node.
   * @param var  Var node nested in the varRef element.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createArrayRef(XbasicType type, Xnode var) {
    Xnode ref = createNode(Xcode.FARRAYREF);
    ref.setAttribute(Xattr.TYPE, type.getRef());
    Xnode varRef = createNode(Xcode.VARREF);
    varRef.setAttribute(Xattr.TYPE, type.getType());
    varRef.append(var);
    ref.append(varRef);
    return ref;
  }

  /**
   * Create a new Id node with all the underlying needed node and attributes.
   *
   * @param type      Value for the attribute type.
   * @param sclass    Value for the attribute sclass.
   * @param nameValue Value of the name inner element.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xid createId(String type, String sclass, String nameValue) {
    Xnode id = createNode(Xcode.ID);
    Xnode internalName = createNode(Xcode.NAME);
    internalName.setValue(nameValue);
    id.append(internalName);
    id.setAttribute(Xattr.TYPE, type);
    id.setAttribute(Xattr.SCLASS, sclass);
    return new Xid(id.element());
  }

  /**
   * Create a new varDecl node with all the mandatory nodes.
   *
   * @param nameType  Value for the attribute type of the name node.
   * @param nameValue Value of the name inner node.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVarDecl(String nameType, String nameValue) {
    Xnode varD = createNode(Xcode.VARDECL);
    Xnode internalName = createNode(Xcode.NAME);
    internalName.setValue(nameValue);
    internalName.setAttribute(Xattr.TYPE, nameType);
    varD.append(internalName);
    return varD;
  }

  /**
   * Constructs a new basicType node with attributes.
   *
   * @param type   Type attribute value.
   * @param ref    Reference attribute value.
   * @param intent Optional intent value.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public XbasicType createBasicType(String type, String ref, Xintent intent) {
    Xnode bt = createNode(Xcode.FBASICTYPE);
    bt.setAttribute(Xattr.TYPE, type);
    if(ref != null) {
      bt.setAttribute(Xattr.REF, ref);
    }
    if(intent != null) {
      bt.setAttribute(Xattr.INTENT, intent.toString());
    }
    return new XbasicType(bt.element());
  }

  /**
   * Create an empty assumed shape indexRange node.
   *
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createEmptyAssumedShaped() {
    Xnode range = createNode(Xcode.INDEXRANGE);
    range.setBooleanAttribute(Xattr.IS_ASSUMED_SHAPE, true);
    return range;
  }

  /**
   * Create a new FdoStatement node with an empty body.
   * <p>
   * <pre>
   * {@code
   *
   * <FdoStatement>
   *   <Var></Var> <!-- provided as argument -->
   *   <indexRange></indexRange> <!-- provided as argument -->
   *   <body></body>
   * </FdoStatement>
   *
   * }
   * </pre>
   *
   * @param inductionVar Var element for the induction variable.
   * @param indexRange   indexRange element for the iteration range.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createDoStmt(Xnode inductionVar, Xnode indexRange) {
    Xnode root = createNode(Xcode.FDOSTATEMENT);
    root.append(inductionVar);
    root.append(indexRange);
    Xnode body = createNode(Xcode.BODY);
    root.append(body);
    return root;
  }

  /**
   * Create a new FifStatement node with an empty condition and then body.
   * <p>
   * <pre>
   * {@code
   *
   * <FifStatement>
   *   <condition></condition>
   *   <then>
   *     <body></body>
   *   </then>
   * </FifStatement>
   *
   * }
   * </pre>
   *
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createIfThen() {
    Xnode root = createNode(Xcode.FIFSTATEMENT);
    Xnode cond = createNode(Xcode.CONDITION);
    Xnode thenBlock = createNode(Xcode.THEN);
    Xnode thenBody = createNode(Xcode.BODY);
    thenBlock.append(thenBody);
    root.append(cond);
    root.append(thenBlock);
    return root;
  }

  /**
   * Create an indexRange element to loop over an assumed shape array.
   *
   * @param arrayVar   Var element representing the array variable.
   * @param startIndex Lower bound index value.
   * @param dimension  Dimension index for the upper bound value.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createRangeForAssumedShapeArray(Xnode arrayVar,
                                               int startIndex,
                                               int dimension)
  {
    // Base structure
    Xnode indexRange = createNode(Xcode.INDEXRANGE);
    Xnode lower = createNode(Xcode.LOWERBOUND);
    Xnode upper = createNode(Xcode.UPPERBOUND);
    indexRange.append(lower);
    indexRange.append(upper);

    // Lower bound
    Xnode lowerBound = createNode(Xcode.FINTCONSTANT);
    lowerBound.setValue(String.valueOf(startIndex));
    lower.append(lowerBound);

    // Upper bound
    Xnode fctCall = createNode(Xcode.FUNCTIONCALL);
    upper.append(fctCall);
    fctCall.setBooleanAttribute(Xattr.IS_INTRINSIC, true);
    fctCall.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
    Xnode name = createNode(Xcode.NAME);
    name.setValue(Xname.INTRINSIC_SIZE);
    fctCall.append(name);
    Xnode args = createNode(Xcode.ARGUMENTS);
    fctCall.append(args);
    args.append(arrayVar, true);
    Xnode dim = createNode(Xcode.FINTCONSTANT);
    dim.setValue(String.valueOf(dimension));
    args.append(dim);
    return indexRange;
  }

  /**
   * Create a name node and adds it as a parameter of the given function
   * type. If the function has optional parameters, the newly created parameter
   * is added before the optional ones.
   *
   * @param nameValue Value of the name node to create.
   * @param type      Type of the name node to create.
   * @param fctType   Function type in which the node will be added as a
   *                  parameter.
   */
  public Xnode createAndAddParam(String nameValue, String type,
                                 XfunctionType fctType)
  {
    Xnode newParam = createName(nameValue, type);
    Xnode hook = null;
    // Newly created parameter must be added before any optional parameter
    for(Xnode param : fctType.getParams().getAll()) {
      XbasicType paramType =
          (XbasicType) getTypeTable().get(param);
      if(paramType.getBooleanAttribute(Xattr.IS_OPTIONAL)) {
        hook = param;
        break;
      }
    }
    if(hook == null) {
      fctType.getParams().add(newParam);
    } else {
      fctType.getParams().addBefore(hook, newParam);
    }
    return newParam;
  }

  /**
   * Create a name node and adds it as a parameter of the given function
   * type if this parameter does not exist yet.
   *
   * @param nameValue Value of the name node to create.
   * @param type      Type of the name node to create.
   * @param fctType   Function type in which the node will be added as a
   *                  parameter.
   */
  public Xnode createAndAddParamIfNotExists(String nameValue, String type,
                                            XfunctionType fctType)
  {
    for(Xnode p : fctType.getParams().getAll()) {
      if(p.value().equals(nameValue.toLowerCase())) {
        return null;
      }
    }
    return createAndAddParam(nameValue, type, fctType);
  }

  /**
   * Create a print statement with the given char constants.
   *
   * @param format        Format for the print statement.
   * @param charConstants Array of char constants to be created.
   * @return The print statement node created.
   */
  public Xnode createPrintStatement(String format, String[] charConstants) {
    Xnode printStatement = createNode(Xcode.FPRINTSTATEMENT);
    printStatement.setAttribute(Xattr.FORMAT, format);

    Xnode valueList = createNode(Xcode.VALUELIST);
    for(String charConstant : charConstants) {
      // Create the char constant type
      String charTypeHash = getTypeTable().generateCharTypeHash();
      Xtype charType = createBasicType(charTypeHash, Xname.F_CHAR_REF, null);
      Xnode len = createNode(Xcode.LEN);
      Xnode intConstant = createNode(Xcode.FINTCONSTANT);
      intConstant.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
      intConstant.setValue(charConstant.length() + "");
      len.append(intConstant);
      charType.append(len);
      getTypeTable().add(charType);

      // Create the value element to be added to the list
      Xnode valueElement = createNode(Xcode.VALUE);
      Xnode fCharElement = createNode(Xcode.FCHARACTERCONSTANT);
      fCharElement.setAttribute(Xattr.TYPE, charTypeHash);
      fCharElement.setValue(charConstant);
      valueElement.append(fCharElement);
      valueList.append(valueElement);
    }
    printStatement.append(valueList);
    return printStatement;
  }

  /**
   * Create a FintConstant node with the given value.
   *
   * @param value Value assigned to the int constant.
   * @return Newly created node.
   */
  public Xnode createIntConstant(int value) {
    Xnode n = createNode(Xcode.FINTCONSTANT);
    n.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
    n.setValue(String.valueOf(value));
    return n;
  }
}
