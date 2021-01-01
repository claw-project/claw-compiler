/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.xml.XMLConstants;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import claw.tatsu.TatsuConstant;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.primitive.Pragma;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.DeclarationPosition;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import claw.tatsu.xcodeml.xnode.fortran.Xintrinsic;

/**
 * The XcodeML class represents the basic XcodeML file unit. Both XcodeProgram
 * and FortranModule inherit from this class.
 *
 * @author clementval
 */
public class XcodeML extends Xnode
{

    private final XtypeTable _typeTable;
    private Document _xcodemlDoc;

    /**
     * Default ctor used for unvalid XcodeML.
     */
    protected XcodeML()
    {
        super(null);
        _typeTable = null;
    }

    /**
     * Constructs a basic XcodeML object representing the XcodeML file given in
     * input.
     *
     * @param baseElement Document representing the XcodeML file.
     */
    protected XcodeML(Document baseElement)
    {
        super(baseElement.getDocumentElement());
        _typeTable = new XtypeTable(matchSeq(Xcode.TYPE_TABLE));
        _xcodemlDoc = baseElement;
    }

    /**
     * Read XML from stream.
     *
     * @param input Xml file path.
     * @return Document if the XML stream could be read. Null otherwise.
     */
    static Document readXmlStream(InputStream input)
    {
        try
        {
            if (input == null)
            {
                return null;
            }
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document doc = dBuilder.parse(input);
            doc.getDocumentElement().normalize();
            return doc;
        } catch (Exception ignored)
        {
        }
        return null;
    }

    /**
     * Create a character type based on the given string
     *
     * @param charConstant String to based the character type on.
     * @return Newly FbasicType created.
     */
    private FbasicType createCharType(String charConstant)
    {
        // Create the char constant type
        FbasicType charType = createBasicType(FortranType.CHARACTER, Intent.NONE);
        charType.append(createNode(Xcode.LEN).append(createIntConstant(charConstant.length())));
        getTypeTable().add(charType);
        return charType;
    }

    /**
     * Create a character constant.
     *
     * @param value String to be placed as the character constant.
     * @return Newly created FcharacterConstant node.
     */
    public Xnode createCharConstant(String value)
    {
        return createNode(Xcode.F_CHARACTER_CONSTANT).setType(createCharType(value)).setValue(value);
    }

    /**
     * Create a new node in the current translation unit.
     *
     * @param opcode Opcode of the new node.
     * @return Newly created node.
     */
    public Xnode createNode(Xcode opcode)
    {
        return new Xnode(getDocument().createElement(opcode.code()));
    }

    /**
     * Create a comment node with its string value. Should not be modified.
     *
     * @param value String value of the comment.
     * @return Newly created comment node.
     */
    public Xnode createComment(String value)
    {
        return createNode(Xcode.F_PRAGMA_STATEMENT).setValue(String.format("cdir %s", value));
    }

    /**
     * @return The XML Document representing the XcodeML file.
     */
    public Document getDocument()
    {
        return _xcodemlDoc;
    }

    /**
     * Get the type table of the FortranModule module.
     *
     * @return The types table.
     */
    public XtypeTable getTypeTable()
    {
        return _typeTable;
    }

    /**
     * Retrieve all function definitions in the XcodeProgram unit.
     *
     * @return A list of all function definitions in the XcodeProgram unit.
     */
    public List<FfunctionDefinition> getAllFctDef()
    {
        return matchAll(Xcode.F_FUNCTION_DEFINITION).stream().map(FfunctionDefinition::new)
                .collect(Collectors.toList());
    }

    /**
     * Create a copy of a binary expression from a XcodeML unit to this unit.
     *
     * @param base       Base element to be copied.
     * @param xcodemlSrc Source XcodeML unit.
     * @return The newly created element in the current XcodeML unit.
     * @throws IllegalTransformationException If the variable element doesn't meet
     *                                        the criteria.
     */
    public Xnode importBinaryExpr(Xnode base, XcodeML xcodemlSrc) throws IllegalTransformationException
    {
        Xnode lhs = base.child(Xnode.LHS);
        Xnode rhs = base.child(Xnode.RHS);
        Xnode binaryExpr = createNode(base.opcode());
        binaryExpr.append(importElement(lhs, xcodemlSrc));
        binaryExpr.append(importElement(rhs, xcodemlSrc));
        return binaryExpr;
    }

    /**
     * Create a copy of an element from a XcodeML unit to this unit.
     *
     * @param base       Base element to be copied.
     * @param xcodemlSrc Source XcodeML unit.
     * @return The newly created element in the current XcodeML unit.
     * @throws IllegalTransformationException If the variable element doesn't meet
     *                                        the criteria.
     */
    public Xnode importElement(Xnode base, XcodeML xcodemlSrc) throws IllegalTransformationException
    {

        if (base == null)
        {
            throw new IllegalTransformationException("Impossible to import null element!");
        }

        switch (base.opcode())
        {
        case VAR:
            return importVar(base, xcodemlSrc);
        case F_INT_CONSTANT:
            return createIntConstant(Integer.parseInt(base.value()));
        case PLUS_EXPR:
        case MINUS_EXPR:
        case DIV_EXPR:
        case MUL_EXPR:
            return importBinaryExpr(base, xcodemlSrc);
        default:
            throw new IllegalTransformationException(
                    String.format("Lower/upper bound type currently not supported (%s)", base.opcode().toString()));
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
    private Xnode importVar(Xnode base, XcodeML xcodemlSrc) throws IllegalTransformationException
    {
        String typeValue = base.getType();
        if (!FortranType.INTEGER.isOfType(typeValue))
        {
            throw new IllegalTransformationException(
                    "Only integer variable are " + "supported as lower/upper bound value for promoted arrays.");
        }

        FbasicType type = xcodemlSrc.getTypeTable().getBasicType(typeValue);
        FbasicType bType = createBasicType(FortranType.INTEGER, Intent.NONE);
        if (type != null)
        {
            bType.setIntent(type.getIntent());
        }

        return createVar(bType.getType(), base.value(), Xscope.fromString(base.getAttribute(Xattr.SCOPE)));
    }

    /**
     * Import a type description from one XcodeML unit to the current one. If the
     * type id is not present in the source XcodeML unit, nothing is done.
     *
     * @param src    Source XcodeML unit.
     * @param typeId Type id to be imported.
     */
    public void importType(XcodeML src, String typeId)
    {
        if (typeId == null || getTypeTable().hasType(typeId))
        {
            return;
        }
        Xnode type = src.getTypeTable().get(typeId);
        if (type == null)
        {
            return;
        }

        Xnode importedType = importNode(type);
        getTypeTable().add(importedType);
        if (importedType.hasAttribute(Xattr.REF) && !FortranType.isBuiltInType(importedType.getAttribute(Xattr.REF)))
        {
            importType(src, importedType.getAttribute(Xattr.REF));
        }

        // Handle possible type ref in indexRange element
        List<Xnode> vars = importedType.matchAll(Xcode.VAR);
        for (Xnode var : vars)
        {
            importType(src, var.getType());
        }
    }

    /**
     * Import node to current XcodeML translation unit.
     *
     * @param node Node to import.
     * @return Imported node.
     */
    public Xnode importNode(Xnode node)
    {
        return new Xnode((Element) getDocument().importNode(node.element(), true));
    }

    /**
     * Write the XcodeML to output stream
     *
     * @param outputStream Output stream
     * @param indent       Number of spaces used for the indentation
     * @throws IllegalTransformationException if XML file cannot be written.
     */
    public void write(OutputStream outputStream, int indent) throws IllegalTransformationException
    {
        try
        {
            cleanEmptyTextNodes(this.getDocument());
            TransformerFactory factory = TransformerFactory.newInstance();
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "");
            factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_STYLESHEET, "");
            Transformer transformer = factory.newTransformer();
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", Integer.toString(indent));
            DOMSource source = new DOMSource(this.getDocument());
            StreamResult streamResult = new StreamResult(outputStream);
            transformer.transform(source, streamResult);
        } catch (Exception e)
        {
            throw new IllegalTransformationException("Failed to transform XCodeML into text", e);
        }
    }

    /**
     * Removes text nodes that only contains whitespace. The conditions for removing
     * text nodes, besides only containing whitespace, are: If the parent node has
     * at least one child of any of the following types, all whitespace-only
     * text-node children will be removed: - ELEMENT child - CDATA child - COMMENT
     * child.
     *
     * @param parentNode Root node to start the cleaning.
     */
    private void cleanEmptyTextNodes(Node parentNode)
    {
        boolean removeEmptyTextNodes = false;
        Node childNode = parentNode.getFirstChild();
        while (childNode != null)
        {
            removeEmptyTextNodes |= checkNodeTypes(childNode);
            childNode = childNode.getNextSibling();
        }

        if (removeEmptyTextNodes)
        {
            removeEmptyTextNodes(parentNode);
        }
    }

    /**
     * Remove all empty text nodes in the subtree.
     *
     * @param parentNode Root node to start the search.
     */
    private void removeEmptyTextNodes(Node parentNode)
    {
        Node childNode = parentNode.getFirstChild();
        while (childNode != null)
        {
            // grab the "nextSibling" before the child node is removed
            Node nextChild = childNode.getNextSibling();
            short nodeType = childNode.getNodeType();
            if (nodeType == Node.TEXT_NODE)
            {
                boolean containsOnlyWhitespace = childNode.getNodeValue().trim().isEmpty();
                if (containsOnlyWhitespace)
                {
                    parentNode.removeChild(childNode);
                }
            }
            childNode = nextChild;
        }
    }

    /*
     * Node creation section
     */

    /**
     * Check the type of the given node.
     *
     * @param childNode Node to be checked.
     * @return True if the node contains data. False otherwise.
     */
    private boolean checkNodeTypes(Node childNode)
    {
        short nodeType = childNode.getNodeType();
        if (nodeType == Node.ELEMENT_NODE)
        {
            cleanEmptyTextNodes(childNode); // recurse into subtree
        }
        return nodeType == Node.ELEMENT_NODE || nodeType == Node.CDATA_SECTION_NODE || nodeType == Node.COMMENT_NODE;
    }

    /**
     * Constructs a new name node with name value and optional type.
     *
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
        return createNode(Xcode.NAME).setValue(value).setType(type);
    }

    /**
     * Create the id and varDecl nodes and add them to the symbol/declaration table.
     *
     * @param name    Name of the variable.
     * @param type    Type of the variable.
     * @param sclass  Scope class of the variable (from Xname).
     * @param fctDef  Function definition in which id and decl are created.
     * @param declPos Position of the newly inserted declaration.
     */
    public void createIdAndDecl(String name, FortranType type, XstorageClass sclass, FfunctionDefinition fctDef,
            DeclarationPosition declPos)
    {
        createIdAndDecl(name, type.toString(), sclass, fctDef, declPos);
    }

    /**
     * Create the id and varDecl nodes and add them to the symbol/declaration table.
     *
     * @param name    Name of the variable.
     * @param type    Type of the variable.
     * @param sclass  Scope class of the variable (from Xname).
     * @param fctDef  Function definition in which id and decl are created.
     * @param declPos Position of the newly inserted declaration.
     */
    public void createIdAndDecl(String name, String type, XstorageClass sclass, FfunctionDefinition fctDef,
            DeclarationPosition declPos)
    {
        Xid id = createId(type, sclass, name);
        fctDef.getSymbolTable().add(id);
        Xnode decl = createVarDecl(type, name);
        Xnode hook = null;

        // Check where is the last dummy arguments in the declaration
        if (declPos == DeclarationPosition.AFTER_DUMMY)
        {
            FfunctionType fctType = getTypeTable().getFunctionType(fctDef);
            List<String> parameters = fctType.getParamsNames();

            for (Xnode n : fctDef.getDeclarationTable().values())
            {
                if (n.is(Xcode.VAR_DECL))
                {
                    String varId = n.matchDirectDescendant(Xcode.NAME).value();
                    if (n.lineNo() == 0 || varId.equalsIgnoreCase(fctDef.getName()))
                    {
                        continue;
                    }
                    if (parameters.contains(varId.toLowerCase()))
                    {
                        hook = n;
                    } else
                    {
                        break;
                    }
                }
            }
        } else if (declPos == DeclarationPosition.FIRST)
        {
            for (Xnode n : fctDef.getDeclarationTable().values())
            {
                if (n.is(Xcode.F_USE_DECL) || n.is(Xcode.F_USE_ONLY_DECL))
                {
                    hook = n;
                } else
                {
                    break;
                }
            }
        }

        // Insert the new declaration
        if (declPos == DeclarationPosition.FIRST && hook == null)
        {
            fctDef.getDeclarationTable().addFirst(decl);
        } else if (hook == null)
        {
            fctDef.getDeclarationTable().add(decl);
        } else
        {
            hook.insertAfter(decl);
        }
    }

    /**
     * Create a new namedValue node with its attribute.
     *
     * {@code
     * <namedValue name="value"></namedValue>
     * }
     *
     * @param value Value of the name attribute.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xnode createNamedValue(String value)
    {
        return createNode(Xcode.NAMED_VALUE).setAttribute(Xattr.NAME, value);
    }

    /**
     * Create a new var node.
     *
     * {@code
     * <Var type="type" scope="scope">value</Var>
     * }
     *
     * @param type  Value of the type attribute.
     * @param value Value of the var.
     * @param scope Value of the scope attribute.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xnode createVar(FortranType type, String value, Xscope scope)
    {
        return createVar(type.toString(), value, scope);
    }

    /**
     * Create a new var node.
     *
     * {@code
     * <Var type="type" scope="scope">value</Var>
     * }
     *
     * @param type  Value of the type attribute.
     * @param value Value of the var.
     * @param scope Value of the scope attribute.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xnode createVar(String type, String value, Xscope scope)
    {
        return createNode(Xcode.VAR).setType(type).setAttribute(Xattr.SCOPE, scope.toString()).setValue(value);
    }

    /**
     * Create a new functionCall node with name and arguments as children nodes.
     *
     * {@code
     * <functionCall type="returnType">
     * <name type="fctType">fctName</name>
     * <arguments></arguments>
     * </functionCall>
     * }
     *
     * @param fctType FfunctionType holding the return type and the type
     *                information.
     * @param fctName Value of the name node.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public FunctionCall createFctCall(FfunctionType fctType, String fctName)
    {
        return createFctCall(fctType.getReturnType(), fctName, fctType.getType());
    }

    /**
     * Create a new functionCall node with name and arguments as children nodes.
     *
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
    public FunctionCall createFctCall(String returnType, String fctName, String fctType)
    {
        Xnode fctCall = createNode(Xcode.FUNCTION_CALL).setType(returnType);
        Xnode fctNameNode = createNode(Xcode.NAME).setValue(fctName.toLowerCase()).setType(fctType);
        fctCall.append(fctNameNode).append(createNode(Xcode.ARGUMENTS));
        return new FunctionCall(fctCall);
    }

    /**
     * Create a new functionCall node with name and arguments as children nodes.
     *
     * {@code
     * <functionCall type="returnType">
     * <name type="fctType">fctName</name>
     * <arguments></arguments>
     * </functionCall>
     * }
     *
     * @param returnType Value of the type attribute for the functionCall node.
     * @param fctName    Value of the name node.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public FunctionCall createIntrinsicFctCall(FortranType returnType, Xintrinsic fctName)
    {
        FunctionCall fctCall = createFctCall(returnType.toString(), fctName.toString(), null);
        fctCall.setBooleanAttribute(Xattr.IS_INTRINSIC, true);
        return fctCall;
    }

    /**
     * Create a FfunctionType representing a subroutine and add it to the type
     * table.
     *
     * @return Newly created node.
     */
    public FfunctionType createSubroutineType()
    {
        FfunctionType subroutine = createFunctionType(getTypeTable().generateHash(FortranType.FUNCTION),
                Xname.TYPE_F_VOID);
        getTypeTable().add(subroutine);
        return subroutine;
    }

    /**
     * Create a new FfunctionType with empty params child.
     *
     * {@code
     * <FfunctionType return_type="returnType" type="type">
     * <params></params>
     * </FfunctionType>
     * }
     *
     * @param type Type hash of the FfunctionType node.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public FfunctionType createFunctionType(String type, String returnType)
    {
        Xnode functionType = createNode(Xcode.F_FUNCTION_TYPE);
        functionType.setType(type);
        if (returnType != null)
        {
            functionType.setAttribute(Xattr.RETURN_TYPE, returnType);
        } else
        {
            functionType.setAttribute(Xattr.RETURN_TYPE, FortranType.VOID.toString());
        }
        functionType.append(createNode(Xcode.PARAMS));
        return new FfunctionType(functionType);
    }

    /**
     * Create a new FfunctionType with a new generated hash with empty params child.
     *
     * {@code
     * <FfunctionType return_type="returnType" type="type">
     * <params></params>
     * </FfunctionType>
     * }
     *
     * @return The newly created node detached in the current XcodeML unit.
     */
    public FfunctionType createFunctionType(String returnType)
    {
        return createFunctionType(getTypeTable().generateHash(FortranType.FUNCTION), returnType);
    }

    /**
     * Create a new FarrayRef node with varRef node as a child with the given Var
     * element.
     *
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
    public Xnode createArrayRef(FbasicType type, Xnode var)
    {
        Xnode varRef = createNode(Xcode.VAR_REF).setType(type.getType()).append(var);
        return createNode(Xcode.F_ARRAY_REF).setType(type.getRef()).append(varRef);
    }

    /**
     * Create a new Id node with all the underlying needed node and attributes.
     *
     * {@code
     * <id type="type" sclass="sclass">idValue</id>
     * }
     *
     * @param type    Value for the attribute type.
     * @param sclass  Value for the attribute sclass.
     * @param idValue Value of the name inner element.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xid createId(FortranType type, XstorageClass sclass, String idValue)
    {
        return createId(type.toString(), sclass, idValue);
    }

    /**
     * Create a new Id node with all the underlying needed node and attributes.
     *
     * {@code
     * <id type="type" sclass="sclass">idValue</id>
     * }
     *
     * @param type    Value for the attribute type.
     * @param sclass  Value for the attribute sclass.
     * @param idValue Value of the name inner element.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xid createId(String type, XstorageClass sclass, String idValue)
    {
        Xnode id = createNode(Xcode.ID).setType(type).setAttribute(Xattr.SCLASS, sclass.toString())
                .append(createNode(Xcode.NAME).setValue(idValue));
        return new Xid(id);
    }

    /**
     * Create a new varDecl node with all the mandatory nodes.
     *
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
    public Xnode createVarDecl(FortranType nameType, String nameValue)
    {
        return createVarDecl(nameType.toString(), nameValue);
    }

    /**
     * Create a new varDecl node with all the mandatory nodes.
     *
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
    public Xnode createVarDecl(String varType, String varId)
    {
        return createNode(Xcode.VAR_DECL).append(createNode(Xcode.NAME).setValue(varId).setType(varType));
    }

    /**
     * Constructs a new basicType node with attributes.
     *
     * {@code
     * <FbasicType type="type" ref="ref" intent="intent"/>
     * }
     *
     * @param type   Reference built-in type.
     * @param intent Optional intent value.
     * @return Newly create FbasicType with a corresponding generated type hash
     *         value.
     */
    public FbasicType createBasicType(FortranType type, Intent intent)
    {
        return createBasicType(getTypeTable().generateHash(type), type.toString(), intent);
    }

    /**
     * Constructs a new basicType node with attributes.
     *
     * {@code
     * <FbasicType type="type" ref="ref" intent="intent"/>
     * }
     *
     * @param type   Type attribute value.
     * @param ref    Reference attribute value.
     * @param intent Optional intent value.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public FbasicType createBasicType(String type, String ref, Intent intent)
    {
        FbasicType bt = new FbasicType(createNode(Xcode.F_BASIC_TYPE));
        bt.setType(type);
        if (ref != null && !ref.isEmpty())
        {
            bt.setRef(ref);
        }
        if (intent != null && intent != Intent.NONE)
        {
            bt.setAttribute(Xattr.INTENT, intent.toString());
        }
        return bt;
    }

    /**
     * Create an empty assumed shape indexRange node.
     *
     * {@code
     * <indexRange is_assumed_shape="true"></indexRange>
     * }
     *
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xnode createEmptyAssumedShaped()
    {
        return createNode(Xcode.INDEX_RANGE).setBooleanAttribute(Xattr.IS_ASSUMED_SHAPE, true);
    }

    /**
     * Create a new FdoStatement node with an empty body.
     *
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
    public Xnode createDoStmt(Xnode inductionVar, Xnode indexRange)
    {
        return createNode(Xcode.F_DO_STATEMENT).append(inductionVar).append(indexRange).append(createNode(Xcode.BODY));
    }

    /**
     * Create a new FifStatement node with an empty condition and then body.
     *
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
    public Xnode createIfThen()
    {
        Xnode thenNode = createNode(Xcode.THEN).append(createNode(Xcode.BODY));
        return createNode(Xcode.F_IF_STATEMENT).append(createNode(Xcode.CONDITION)).append(thenNode);
    }

    /**
     * Create a new else node with a body node inside.
     *
     * @return The newly create node detached in the current XcodeML unit.
     */
    public Xnode createElse()
    {
        return createNode(Xcode.ELSE).append(createNode(Xcode.BODY));
    }

    /**
     * Create an indexRange element to loop over an assumed shape array.
     *
     * @param arrayVar   Var element representing the array variable.
     * @param startIndex Lower bound index value.
     * @param dimension  Dimension index for the upper bound value.
     * @return The newly created node detached in the current XcodeML unit.
     */
    public Xnode createRangeForAssumedShapeArray(Xnode arrayVar, int startIndex, int dimension)
    {
        // Lower bound
        Xnode lower = createNode(Xcode.LOWER_BOUND).append(createIntConstant(startIndex));

        // Upper bound
        FunctionCall fctCall = createIntrinsicFctCall(FortranType.INTEGER, Xintrinsic.SIZE);
        fctCall.addArguments(arrayVar.cloneNode());
        fctCall.addArguments(createIntConstant(dimension));
        Xnode upper = createNode(Xcode.UPPER_BOUND).append(fctCall);

        return createNode(Xcode.INDEX_RANGE).append(lower).append(upper);
    }

    /**
     * Create a name node and adds it as a parameter of the given function type. If
     * the function has optional parameters, the newly created parameter is added
     * before the optional ones.
     *
     * @param nameValue Value of the name node to create.
     * @param type      Type of the name node to create.
     * @param fctType   Function type in which the node will be added as a
     *                  parameter.
     * @return newly created name element.
     */
    public Xnode createAndAddParam(String nameValue, String type, FfunctionType fctType)
    {
        Xnode newParam = createName(nameValue, type);
        Xnode hook = null;
        // Newly created parameter must be added before any optional parameter
        for (Xnode param : fctType.getParameters())
        {
            FbasicType paramType = getTypeTable().getBasicType(param);
            if (paramType.getBooleanAttribute(Xattr.IS_OPTIONAL))
            {
                hook = param;
                break;
            }
        }
        if (hook == null)
        {
            fctType.addParameters(newParam);
        } else
        {
            fctType.addParameters(hook, newParam);
        }
        return newParam;
    }

    /**
     * Create a name node and adds it as a parameter of the given function type if
     * this parameter does not exist yet.
     *
     * @param nameValue Value of the name node to create.
     * @param type      Type of the name node to create.
     * @param fctType   Function type in which the node will be added as a
     *                  parameter.
     * @return newly created name element or null if not created.
     */
    public Xnode createAndAddParamIfNotExists(String nameValue, String type, FfunctionType fctType)
    {
        if (fctType.isProgram())
        {
            return null;
        }

        if (fctType.getParameters().stream().map(Xnode::value).anyMatch(nameValue::equalsIgnoreCase))
        {
            return null;
        }
        return createAndAddParam(nameValue, type, fctType);
    }

    /**
     * Create a print statement with the given char constants.
     *
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
    public Xnode createPrintStatement(String format, String[] charConstants)
    {
        Xnode printStatement = createNode(Xcode.F_PRINT_STATEMENT);
        printStatement.setAttribute(Xattr.FORMAT, format);

        Xnode valueList = createNode(Xcode.VALUE_LIST);
        for (String charConstant : charConstants)
        {
            // Create the char constant type
            Xnode charType = createBasicType(FortranType.CHARACTER, Intent.NONE);
            Xnode len = createNode(Xcode.LEN);
            len.append(createIntConstant(charConstant.length()));
            charType.append(len);
            getTypeTable().add(charType);

            // Create the value element to be added to the list
            Xnode valueElement = createNode(Xcode.VALUE);
            Xnode fCharElement = createNode(Xcode.F_CHARACTER_CONSTANT);
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
     *
     * {@code
     * <FintConstant type="Fint">value</FintConstant>
     * }
     *
     * @param value Value assigned to the int constant.
     * @return Newly created node.
     */
    public Xnode createIntConstant(int value)
    {
        return createNode(Xcode.F_INT_CONSTANT).setType(Xname.TYPE_F_INT).setValue(String.valueOf(value));
    }

    /**
     * Create a single FpragmaStatement with the given value.
     *
     * @param value Value of the created pragma.
     * @return Created node.
     */
    public Xnode createSinglePragma(String value)
    {
        return createNode(Xcode.F_PRAGMA_STATEMENT).setValue(value);
    }

    /**
     * Create a list of FpragmaStatement with correct line continuation symbols.
     * Initial value is splitted according to the max column information. This is
     * done as the OMNI Compiler backend doesn't split the FpragmaElement.
     *
     * Supports acc, omp and claw prefix
     *
     * @param value     Value of the FpragmaStatement.
     * @param maxColumn Maximum column value. Split is based on this value.
     * @return List of FpragmaStatement representing the splitted value.
     */
    public List<Xnode> createPragma(String value, int maxColumn)
    {
        if (value == null || value.isEmpty())
        {
            return Collections.emptyList();
        }
        List<Xnode> pragmas = new ArrayList<>();
        value = value.trim().toLowerCase();
        String prefix = "";
        for (CompilerDirective dir : CompilerDirective.values())
        {
            if (dir != CompilerDirective.NONE && value.startsWith(dir.getPrefix()))
            {
                prefix = dir.getPrefix();
                break;
            }
        }

        List<String> chunks = Pragma.split(value, maxColumn, prefix);
        for (int i = 0; i < chunks.size(); ++i)
        {
            String chunk = chunks.get(i).trim();
            Xnode p = createNode(Xcode.F_PRAGMA_STATEMENT);
            if (i == chunks.size() - 1)
            { // Last chunk
                if (!chunk.startsWith(prefix))
                {
                    p.setValue(prefix + " " + chunk);
                } else
                {
                    p.setValue(chunk);
                }
            } else
            {
                if (!chunk.startsWith(prefix))
                {
                    p.setValue(prefix + " " + chunk + " " + TatsuConstant.CONTINUATION_LINE_SYMBOL);
                } else
                {
                    p.setValue(chunk + " " + TatsuConstant.CONTINUATION_LINE_SYMBOL);
                }
            }
            pragmas.add(p);
        }
        return pragmas;
    }

    /**
     * Create a FuseDecl node with the module name.
     *
     * @param moduleName Module name inserted for the use statement.
     * @return Newly created FuseDecl node.
     */
    public Xnode createUseDecl(String moduleName)
    {
        return createNode(Xcode.F_USE_DECL).setAttribute(Xattr.NAME, moduleName);
    }

    /**
     * Create a FuseOnlyDecl node with the module name and renamable elements.
     *
     * @param moduleName Module name inserted for the use statement.
     * @param names      List of use names to be add in the ONLY list.
     * @return Newly created FuseOnlyDecl node.
     */
    public Xnode createUseOnlyDecl(String moduleName, List<String> names)
    {
        if (names == null || names.isEmpty())
        {
            return createUseDecl(moduleName);
        }
        Xnode useOnlyDecl = createNode(Xcode.F_USE_ONLY_DECL);
        useOnlyDecl.setAttribute(Xattr.NAME, moduleName);
        for (String useName : names)
        {
            Xnode renamable = createNode(Xcode.RENAMABLE);
            renamable.setAttribute(Xattr.USE_NAME, useName);
            useOnlyDecl.append(renamable);
        }
        return useOnlyDecl;
    }
}
