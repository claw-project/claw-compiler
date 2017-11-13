/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.transformation.primitive.Pragma;
import cx2x.xcodeml.exception.IllegalTransformationException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
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
    _typeTable = new XtypeTable(matchSeq(Xcode.TYPETABLE));
    _xcodemlDoc = baseElement;
  }

  /**
   * Create a new node in the current translation unit.
   *
   * @param opcode Opcode of the new node.
   * @return Newly created node.
   */
  public Xnode createNode(Xcode opcode) {
    return new Xnode(getDocument().createElement(opcode.code()));
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
  public List<XfunctionDefinition> getAllFctDef() {
    List<XfunctionDefinition> definitions = new ArrayList<>();
    List<Xnode> nodes = matchAll(Xcode.FFUNCTIONDEFINITION);
    for(Xnode fctDef : nodes) {
      definitions.add(new XfunctionDefinition(fctDef));
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
      return createIntConstant(Integer.parseInt(base.value()));
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
    if(!XcodeType.INTEGER.isOfType(typeValue)) {
      throw new IllegalTransformationException("Only integer variable are " +
          "supported as lower/upper bound value for promoted arrays.");
    }

    XbasicType type = xcodemlSrc.getTypeTable().getBasicType(typeValue);
    XbasicType bType = createBasicType(XcodeType.INTEGER, Xintent.NONE);
    if(type != null) {
      bType.setIntent(type.getIntent());
    }

    return createVar(bType.getType(), base.value(),
        Xscope.fromString(base.getAttribute(Xattr.SCOPE)));
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
    Xnode type = src.getTypeTable().get(typeId);
    if(type == null) {
      return;
    }
    //Node rawNode = getDocument().importNode(type.element(), true);
    //Xnode importedType = new Xnode((Element) rawNode);
    Xnode importedType = importNode(type);
    getTypeTable().add(importedType);
    if(importedType.hasAttribute(Xattr.REF)
        && !XcodeType.isBuiltInType(importedType.getAttribute(Xattr.REF)))
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
   * Import node to current XcodeML translation unit.
   *
   * @param node Node to import.
   * @return Imported node.
   */
  public Xnode importNode(Xnode node) {
    return new Xnode((Element) getDocument().importNode(node.element(), true));
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
   * <p>
   * {@code
   * <name type="type">value</name>
   * }
   *
   * @param value Name value.
   * @param type  Optional type value.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createName(String value, String type)
  {
    Xnode n = createNode(Xcode.NAME);
    n.setValue(value);
    if(type != null && !type.isEmpty()) {
      n.setType(type);
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
  public void createIdAndDecl(String name, XcodeType type,
                              XstorageClass sclass,
                              XfunctionDefinition fctDef,
                              boolean afterDummyArgs)
  {
    createIdAndDecl(name, type.toString(), sclass, fctDef, afterDummyArgs);
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
  public void createIdAndDecl(String name, String type, XstorageClass sclass,
                              XfunctionDefinition fctDef,
                              boolean afterDummyArgs)
  {
    Xid id = createId(type, sclass, name);
    fctDef.getSymbolTable().add(id);
    Xnode decl = createVarDecl(type, name);
    Xnode hook = null;

    // Check where is the last dummy arguments in the declaration
    if(afterDummyArgs) {
      XfunctionType fctType = getTypeTable().getFunctionType(fctDef);
      List<String> parameters = fctType.getParamsNames();

      for(Xnode n : fctDef.getDeclarationTable().values()) {
        if(n.opcode() == Xcode.VARDECL) {
          String varId = n.matchDirectDescendant(Xcode.NAME).value();
          if(n.lineNo() == 0
              || varId.toLowerCase().equals(fctDef.getName()))
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
   * <p>
   * {@code
   * <namedValue name="value"></namedValue>
   * }
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
   * <Var type="type" scope="scope">value</Var>
   * }
   *
   * @param type  Value of the type attribute.
   * @param value Value of the var.
   * @param scope Value of the scope attribute.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVar(XcodeType type, String value, Xscope scope) {
    return createVar(type.toString(), value, scope);
  }

  /**
   * Create a new var node.
   * <p>
   * {@code
   * <Var type="type" scope="scope">value</Var>
   * }
   *
   * @param type  Value of the type attribute.
   * @param value Value of the var.
   * @param scope Value of the scope attribute.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVar(String type, String value, Xscope scope) {
    Xnode var = createNode(Xcode.VAR);
    var.setType(type);
    var.setAttribute(Xattr.SCOPE, scope.toString());
    var.setValue(value);
    return var;
  }

  /**
   * Create a new FunctionCall node with name and arguments as children nodes.
   * <p>
   * {@code
   * <functionCall type="returnType">
   * <name type="fctType">fctName</name>
   * <arguments></arguments>
   * </functionCall>
   * }
   *
   * @param returnType Value of the type attribute for the functionCall node.
   * @param fctName    Value of the name node.
   * @param fctType    Value of the type attribute for the name node.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createFctCall(String returnType, String fctName,
                             String fctType)
  {
    Xnode fctCall = createNode(Xcode.FUNCTIONCALL);
    fctCall.setType(returnType);
    Xnode fctNameNode = createNode(Xcode.NAME);
    fctNameNode.setValue(fctName);
    fctNameNode.setType(fctType);
    fctCall.append(fctNameNode);
    fctCall.append(createNode(Xcode.ARGUMENTS));
    return fctCall;
  }

  /**
   * Create a new FarrayRef node with varRef node as a child with the
   * given Var element.
   * <p>
   * {@code
   * <FarrayRef type="type">
   * <varRef type="">
   * <Var/> <!-- var argument -->
   * </varRef>
   * </FarrayRef>
   * }
   *
   * @param type Value of the type attribute for the FarrayRef node.
   * @param var  Var node nested in the varRef element.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createArrayRef(XbasicType type, Xnode var) {
    Xnode ref = createNode(Xcode.FARRAYREF);
    ref.setType(type.getRef());
    Xnode varRef = createNode(Xcode.VARREF);
    varRef.setType(type.getType());
    varRef.append(var);
    ref.append(varRef);
    return ref;
  }

  /**
   * Create a new Id node with all the underlying needed node and attributes.
   * <p>
   * {@code
   * <id type="type" sclass="sclass">idValue</id>
   * }
   *
   * @param type    Value for the attribute type.
   * @param sclass  Value for the attribute sclass.
   * @param idValue Value of the name inner element.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xid createId(XcodeType type, XstorageClass sclass, String idValue)
  {
    return createId(type.toString(), sclass, idValue);
  }

  /**
   * Create a new Id node with all the underlying needed node and attributes.
   * <p>
   * {@code
   * <id type="type" sclass="sclass">idValue</id>
   * }
   *
   * @param type    Value for the attribute type.
   * @param sclass  Value for the attribute sclass.
   * @param idValue Value of the name inner element.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xid createId(String type, XstorageClass sclass, String idValue) {
    Xnode id = createNode(Xcode.ID);
    Xnode internalName = createNode(Xcode.NAME);
    internalName.setValue(idValue);
    id.append(internalName);
    id.setType(type);
    id.setAttribute(Xattr.SCLASS, sclass.toString());
    return new Xid(id);
  }

  /**
   * Create a new varDecl node with all the mandatory nodes.
   * <p>
   * {@code
   * <varDecl>
   * <name type="varType">varId</name>
   * </varDecl>
   * }
   *
   * @param nameType  Value for the attribute type of the name node.
   * @param nameValue Value of the name inner node.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVarDecl(XcodeType nameType, String nameValue) {
    return createVarDecl(nameType.toString(), nameValue);
  }

  /**
   * Create a new varDecl node with all the mandatory nodes.
   * <p>
   * {@code
   * <varDecl>
   * <name type="varType">varId</name>
   * </varDecl>
   * }
   *
   * @param varType Value for the attribute type of the name node.
   * @param varId   Value of the name inner node.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createVarDecl(String varType, String varId) {
    Xnode varDecl = createNode(Xcode.VARDECL);
    Xnode nameNode = createNode(Xcode.NAME);
    nameNode.setValue(varId);
    nameNode.setType(varType);
    varDecl.append(nameNode);
    return varDecl;
  }

  /**
   * Constructs a new basicType node with attributes.
   * <p>
   * {@code
   * <FbasicType type="type" ref="ref" intent="intent"/>
   * }
   *
   * @param type   Reference built-in type.
   * @param intent Optional intent value.
   * @return Newly create FbasicType with a corresponding generated type hash
   * value.
   */
  public XbasicType createBasicType(XcodeType type, Xintent intent) {
    String typeHash = getTypeTable().generateHash(type);
    return createBasicType(typeHash, type.toString(), intent);
  }

  /**
   * Constructs a new basicType node with attributes.
   * <p>
   * {@code
   * <FbasicType type="type" ref="ref" intent="intent"/>
   * }
   *
   * @param type   Type attribute value.
   * @param ref    Reference attribute value.
   * @param intent Optional intent value.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public XbasicType createBasicType(String type, String ref, Xintent intent) {
    XbasicType bt = new XbasicType(createNode(Xcode.FBASICTYPE));
    bt.setType(type);
    if(ref != null && !ref.isEmpty()) {
      bt.setRef(ref);
    }
    if(intent != null && intent != Xintent.NONE) {
      bt.setAttribute(Xattr.INTENT, intent.toString());
    }
    return bt;
  }

  /**
   * Create an empty assumed shape indexRange node.
   * <p>
   * {@code
   * <indexRange is_assumed_shape="true"></indexRange>
   * }
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
   * <FdoStatement>
   * <Var></Var> <!-- provided as argument -->
   * <indexRange></indexRange> <!-- provided as argument -->
   * <body></body>
   * </FdoStatement>
   * }
   * </pre>
   *
   * @param inductionVar Var element for the induction variable.
   * @param indexRange   indexRange element for the iteration range.
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createDoStmt(Xnode inductionVar, Xnode indexRange) {
    Xnode doStatementNode = createNode(Xcode.FDOSTATEMENT);
    doStatementNode.append(inductionVar);
    doStatementNode.append(indexRange);
    doStatementNode.append(createNode(Xcode.BODY));
    return doStatementNode;
  }

  /**
   * Create a new FifStatement node with an empty condition and then body.
   * <p>
   * <pre>
   * {@code
   * <FifStatement>
   * <condition></condition>
   * <then>
   * <body></body>
   * </then>
   * </FifStatement>
   * }
   * </pre>
   *
   * @return The newly created node detached in the current XcodeML unit.
   */
  public Xnode createIfThen() {
    Xnode ifNode = createNode(Xcode.FIFSTATEMENT);
    Xnode thenNode = createNode(Xcode.THEN);
    thenNode.append(createNode(Xcode.BODY));
    ifNode.append(createNode(Xcode.CONDITION));
    ifNode.append(thenNode);
    return ifNode;
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
    lower.append(createIntConstant(startIndex));

    // Upper bound
    Xnode fctCall = createNode(Xcode.FUNCTIONCALL);
    upper.append(fctCall);
    fctCall.setBooleanAttribute(Xattr.IS_INTRINSIC, true);
    fctCall.setType(Xname.TYPE_F_INT);
    Xnode name = createNode(Xcode.NAME);
    name.setValue(Xname.INTRINSIC_SIZE);
    fctCall.append(name);
    Xnode args = createNode(Xcode.ARGUMENTS);
    fctCall.append(args);
    args.append(arrayVar, true);
    args.append(createIntConstant(dimension));
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
      XbasicType paramType = getTypeTable().getBasicType(param);
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
   * <p>
   * {@code
   * <FprintStatement format="format">
   * <valueList>
   * <value> <!-- a value node by element in charConstants -->
   * <!-- FcharacterConstant type are created on the fly -->
   * <FcharacterConstant type="cType">charConstants</FcharacterConstant>
   * </value>
   * </valueList>
   * </FprintStatement>
   * }
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
      Xnode charType = createBasicType(XcodeType.CHARACTER, Xintent.NONE);
      Xnode len = createNode(Xcode.LEN);
      len.append(createIntConstant(charConstant.length()));
      charType.append(len);
      getTypeTable().add(charType);

      // Create the value element to be added to the list
      Xnode valueElement = createNode(Xcode.VALUE);
      Xnode fCharElement = createNode(Xcode.FCHARACTERCONSTANT);
      fCharElement.setType(charType.getType());
      fCharElement.setValue(charConstant);
      valueElement.append(fCharElement);
      valueList.append(valueElement);
    }
    printStatement.append(valueList);
    return printStatement;
  }

  /**
   * Create a FintConstant node with the given value.
   * <p>
   * {@code
   * <FintConstant type="Fint">value</FintConstant>
   * }
   *
   * @param value Value assigned to the int constant.
   * @return Newly created node.
   */
  public Xnode createIntConstant(int value) {
    Xnode n = createNode(Xcode.FINTCONSTANT);
    n.setType(Xname.TYPE_F_INT);
    n.setValue(String.valueOf(value));
    return n;
  }

  /**
   * Create a list of FpragmaStatement with correct line continuation symbols.
   * Initial value is splitted according to the max column information. This is
   * done as the OMNI Compiler backend doesn't split the FpragmaElement.
   * <p>
   * Supports acc, omp and claw prefix
   *
   * @param value     Value of the FpragmaStatement.
   * @param maxColumn Maximum column value. Split is based on this value.
   * @return List of FpragmaStatement representing the splitted value.
   */
  public List<Xnode> createPragma(String value, int maxColumn) {
    if(value == null || value.isEmpty()) {
      return Collections.emptyList();
    }
    List<Xnode> pragmas = new ArrayList<>();
    value = value.trim().toLowerCase();
    String prefix = "";
    if(value.startsWith(ClawConstant.OPENACC_PREFIX)) {
      prefix = ClawConstant.OPENACC_PREFIX;
    } else if(value.startsWith(ClawConstant.OPENMP_PREFIX)) {
      prefix = ClawConstant.OPENMP_PREFIX;
    } else if(value.startsWith(ClawConstant.CLAW)) {
      prefix = ClawConstant.CLAW;
    }

    List<String> chunks = Pragma.split(value, maxColumn, prefix);
    for(int i = 0; i < chunks.size(); ++i) {
      String chunk = chunks.get(i).trim();
      Xnode p = createNode(Xcode.FPRAGMASTATEMENT);
      if(i == chunks.size() - 1) { // Last chunk
        if(!chunk.startsWith(prefix)) {
          p.setValue(prefix + " " + chunk);
        } else {
          p.setValue(chunk);
        }
      } else {
        if(!chunk.startsWith(prefix)) {
          p.setValue(prefix + " " + chunk + " " +
              ClawConstant.CONTINUATION_LINE_SYMBOL);
        } else {
          p.setValue(chunk + " " + ClawConstant.CONTINUATION_LINE_SYMBOL);
        }
      }
      pragmas.add(p);
    }
    return pragmas;
  }

}
