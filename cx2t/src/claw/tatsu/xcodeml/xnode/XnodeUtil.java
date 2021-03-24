/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode;

import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.abstraction.HoistedNestedDoStatement;
import claw.tatsu.xcodeml.xnode.common.*;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

/**
 * The class XnodeUtil contains only static method to help manipulating the raw
 * Elements in the XcodeML representation by using the abstracted Xnode.
 *
 * @author clementval
 */

public class XnodeUtil
{

    private XnodeUtil()
    {
        // Hide implicit public ctor
    }

    /**
     * Find all array references elements in a given body and give var name.
     *
     * @param parent    The body element to search for the array references.
     * @param arrayName Name of the array for the array reference to be found.
     * @return A list of all array references found.
     */
    public static List<Xnode> getAllArrayReferences(Xnode parent, String arrayName)
    {
        List<Xnode> references = new ArrayList<>();
        NodeList nList = parent.element().getElementsByTagName(Xname.F_ARRAY_REF);
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node n = nList.item(i);
            if (n.getNodeType() == Node.ELEMENT_NODE)
            {
                Xnode ref = new Xnode((Element) n);
                Xnode var = ref.matchSeq(Xcode.VAR_REF, Xcode.VAR);
                if (var != null && var.value().equalsIgnoreCase(arrayName))
                {
                    references.add(ref);
                }
            }
        }
        return references;
    }

    /**
     * Find all var references elements in a given body and give var name.
     *
     * @param parent  The body element to search for the array references.
     * @param varName Name of the var for the reference to be found.
     * @return A list of all references found.
     */
    public static List<Xnode> getAllVarReferences(Xnode parent, String varName)
    {
        List<Xnode> references = new ArrayList<>();
        NodeList nList = parent.element().getElementsByTagName(Xname.VAR);
        for (int i = 0; i < nList.getLength(); i++)
        {
            Node n = nList.item(i);
            if (n.getNodeType() == Node.ELEMENT_NODE)
            {
                Xnode var = new Xnode((Element) n);
                if (var.value().equalsIgnoreCase(varName))
                {
                    references.add(var);
                }
            }
        }
        return references;
    }

    /**
     * Retrieve the index ranges of an array notation.
     *
     * @param arrayRef The array reference statements to extract the ranges from.
     * @return A list if indexRanges elements.
     */
    public static List<Xnode> getIdxRangesFromArrayRef(Xnode arrayRef)
    {
        if (!Xnode.isOfCode(arrayRef, Xcode.F_ARRAY_REF))
        {
            return Collections.emptyList();
        }
        return arrayRef.children().stream().filter(x -> x.is(Xcode.INDEX_RANGE)).collect(Collectors.toList());
    }

    /**
     * Find all array assignment statement from a node until the to node.
     *
     * @param from Node from which the search is initiated.
     * @param to   Node until the search is done.
     * @return A list of all assign statements found. List is empty if no statements
     *         are found.
     */
    public static List<Xnode> getArrayAssignInBlock(Xnode from, Xnode to)
    {
        List<Xnode> assignments = new ArrayList<>();
        Xnode crt = from.nextSibling();
        while (crt != null && !crt.equals(to))
        {
            if (crt.is(Xcode.F_ASSIGN_STATEMENT) && crt.firstChild() != null && crt.firstChild().is(Xcode.F_ARRAY_REF))
            {
                assignments.add(crt);
            }
            crt = crt.nextSibling();
        }
        return assignments;
    }

    /**
     * Get all array references in the siblings of the given element.
     *
     * @param from       Element to start from.
     * @param identifier Array name value.
     * @return List of all array references found. List is empty if nothing is
     *         found.
     */
    public static List<Xnode> getAllArrayReferencesInSiblings(Xnode from, String identifier)
    {
        String s1 = String.format("following-sibling::*//%s[%s[%s[text()=\"%s\"]]]", Xname.F_ARRAY_REF, Xname.VAR_REF,
                Xname.VAR, identifier);
        return getFromXpath(from, s1);
    }

    /**
     * Get the first assignment statement for an array reference.
     *
     * @param from      Statement to look from.
     * @param arrayName Identifier of the array.
     * @return The assignment statement if found. Null otherwise.
     */
    public static Xnode getFirstArrayAssign(Xnode from, String arrayName)
    {
        String s1 = String.format("following::%s[%s[%s[%s[text()=\"%s\"]] and position()=1]]", Xname.F_ASSIGN_STATEMENT,
                Xname.F_ARRAY_REF, Xname.VAR_REF, Xname.VAR, arrayName);

        try
        {
            NodeList output = evaluateXpath(from.element(), s1);
            if (output.getLength() == 0)
            {
                return null;
            }
            Element assign = (Element) output.item(0);
            return new Xnode(assign);
        } catch (XPathExpressionException ignored)
        {
            return null;
        }
    }

    /**
     * Find all the nested do statement groups following the inductions iterations
     * define in inductionVars and being located between the "from" element and the
     * end pragma.
     *
     * @param from          Element from which the search is started.
     * @param endPragma     End pragma that terminates the search block.
     * @param inductionVars Induction variables that define the nested group to
     *                      locates.
     * @return List of do statements elements (outer most loops for each nested
     *         group) found between the "from" element and the end pragma.
     */
    public static List<HoistedNestedDoStatement> findDoStatementForHoisting(Xnode from, Xnode endPragma,
            List<String> inductionVars)
    {

        /*
         * s1 is selecting all the nested do statement groups that meet the criteria
         * from the "from" element down to the end of the block.
         */
        String s1 = "following::";

        String dynamicPartS1 = "";
        for (int i = inductionVars.size() - 1; i >= 0; --i)
        {
            /*
             * Here is example of there xpath query format for 1,2 and 3 nested loops
             *
             * FdoStatement[Var[text()="induction_var"]]
             *
             * FdoStatement[Var[text()="induction_var"] and
             * body[FdoStatement[Var[text()="induction_var"]]]
             *
             * FdoStatement[Var[text()="induction_var"] and
             * body[FdoStatement[Var[text()="induction_var"] and
             * body[FdoStatement[Var[text()="induction_var"]]]]
             */

            String tempQuery;
            if (i == inductionVars.size() - 1)
            { // first iteration
                tempQuery = String.format("%s[%s[text()=\"%s\"]]", Xname.F_DO_STATEMENT, Xname.VAR,
                        inductionVars.get(i));
            } else
            {
                tempQuery = String.format("%s[%s[text()=\"%s\"] and %s[%s]]", Xname.F_DO_STATEMENT, Xname.VAR,
                        inductionVars.get(i), Xname.BODY, dynamicPartS1); // Including previously formed xpath query
            }
            dynamicPartS1 = tempQuery;
        }
        s1 += dynamicPartS1;
        List<HoistedNestedDoStatement> doStatements = new ArrayList<>();
        try
        {
            NodeList output = evaluateXpath(from.element(), s1);
            for (int i = 0; i < output.getLength(); i++)
            {
                Element el = (Element) output.item(i);
                Xnode doStmt = new Xnode(el);
                if (doStmt.lineNo() != 0 && doStmt.lineNo() < endPragma.lineNo() && doStmt.lineNo() > from.lineNo())
                {
                    doStatements.add(new HoistedNestedDoStatement(doStmt, inductionVars.size()));
                }
            }
        } catch (XPathExpressionException ignored)
        {
        }
        return doStatements;
    }

    /**
     * Evaluates an Xpath expression and return its result as a NodeList.
     *
     * @param from  Element to start the evaluation.
     * @param xpath Xpath expression.
     * @return Result of evaluation as a NodeList.
     * @throws XPathExpressionException if evaluation fails.
     */
    private static NodeList evaluateXpath(Element from, String xpath) throws XPathExpressionException
    {
        XPathExpression ex = XPathFactory.newInstance().newXPath().compile(xpath);
        return (NodeList) ex.evaluate(from, XPathConstants.NODESET);
    }

    /**
     * Find all array references in the next children that match the given criteria.
     *
     * This methods use powerful Xpath expression to locate the correct nodes in the
     * AST
     *
     * Here is an example of such a query that return all node that are array
     * references for the array "array6" with an offset of 0 -1
     *
     * //FarrayRef[varRef[Var[text()="array6"]] and arrayIndex and
     * arrayIndex[minusExpr[Var and FintConstant[text()="1"]]]]
     *
     * @param from       The element from which the search is initiated.
     * @param identifier Identifier of the array.
     * @param offsets    List of offsets to be search for.
     * @return A list of all array references found.
     */
    public static List<Xnode> getAllArrayReferencesByOffsets(Xnode from, String identifier, List<Integer> offsets)
    {
        StringBuilder offsetXpath = new StringBuilder();
        for (int i = 0; i < offsets.size(); ++i)
        {
            if (offsets.get(i) == 0)
            {
                offsetXpath.append(String.format("%s[position()=%s and %s]", Xname.ARRAY_INDEX, i + 1, Xname.VAR));
            } else if (offsets.get(i) > 0)
            {
                offsetXpath.append(String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                        Xname.ARRAY_INDEX, i + 1, Xname.MINUS_EXPR, Xname.VAR, Xname.F_INT_CONST, offsets.get(i)));
            } else
            {
                offsetXpath
                        .append(String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]", Xname.ARRAY_INDEX,
                                i + 1, Xname.MINUS_EXPR, Xname.VAR, Xname.F_INT_CONST, Math.abs(offsets.get(i))));
            }
            if (i != offsets.size() - 1)
            {
                offsetXpath.append(" and ");
            }
        }

        // Start of the Xpath query
        String xpathQuery = String.format(".//%s[%s[%s[text()=\"%s\"]] and %s]", Xname.F_ARRAY_REF, Xname.VAR_REF,
                Xname.VAR, identifier, offsetXpath.toString());

        return getFromXpath(from, xpathQuery);
    }

    /**
     * Find all the index elements (arrayIndex and indexRange) in an element.
     *
     * @param parent Root element to search from.
     * @return A list of all index ranges found.
     */
    public static List<Xnode> findIndexes(Xnode parent)
    {
        List<Xnode> indexRanges = new ArrayList<>();
        if (parent == null || parent.element() == null)
        {
            return indexRanges;
        }

        Node node = parent.element().getFirstChild();
        while (node != null)
        {
            if (node.getNodeType() == Node.ELEMENT_NODE)
            {
                Element element = (Element) node;
                if (element.getTagName().equals(Xname.ARRAY_INDEX) || element.getTagName().equals(Xname.INDEX_RANGE))
                {
                    indexRanges.add(new Xnode(element));
                }
            }
            node = node.getNextSibling();
        }

        return indexRanges;
    }

    /**
     * Find all the var names that are real references to a variable. Variable used
     * as array index are excluded.
     *
     * @param parent Root element to search from.
     * @return A set of all variables names.
     */
    public static Set<String> findAllReferences(Xnode parent)
    {
        Set<String> names = new HashSet<>();
        if (Xnode.isOfCode(parent, Xcode.VAR))
        {
            names.add(parent.value());
            return names;
        }

        names.addAll(parent.matchAll(Xcode.VAR).stream().filter(Xnode::isNotArrayIndex).map(Xnode::value)
                .collect(Collectors.toList()));
        return names;
    }

    /**
     * Find all the var elements that are real references to a variable. Var element
     * nested in an arrayIndex element are excluded.
     *
     * @param parent Root element to search from.
     * @param id     Identifier of the var to be found.
     * @return A list of all var elements found.
     */
    public static List<Xnode> findAllReferences(Xnode parent, String id)
    {
        return parent.matchAll(Xcode.VAR).stream().filter(Xnode::isNotArrayIndex)
                .filter(x -> x.value().equalsIgnoreCase(id)).collect(Collectors.toList());
    }

    /**
     * Check whether the index of an arrayIndex element is an induction variable
     * given in the list.
     *
     * @param arrayIndex         arrayIndex to be checked.
     * @param inductionVariables List of potential induction variables.
     * @return True if the arrayIndex uses a var from induction variables list.
     *         False in any other cases.
     */
    public static boolean isInductionIndex(Xnode arrayIndex, List<String> inductionVariables)
    {
        if (!Xnode.isOfCode(arrayIndex, Xcode.ARRAY_INDEX) || inductionVariables == null
                || inductionVariables.isEmpty())
        {
            return false;
        }

        Xnode var = arrayIndex.matchDirectDescendant(Xcode.VAR);
        return var != null && inductionVariables.contains(var.value());
    }

    /**
     * Get given statements in the subtree.
     *
     * @param root        Root of the subtree.
     * @param nodeOpcodes List of statements to look for.
     * @return List of statement found.
     */
    public static List<Xnode> getNodes(Xnode root, List<Xcode> nodeOpcodes)
    {
        if (root == null)
        {
            return Collections.emptyList();
        }
        List<Xnode> unsupportedStatements = new ArrayList<>();
        for (Xcode opcode : nodeOpcodes)
        {
            unsupportedStatements.addAll(root.matchAll(opcode));
        }
        return unsupportedStatements;
    }

    /**
     * Get given statements in between from and to included.
     *
     * @param from        Node from.
     * @param to          Node to.
     * @param nodeOpCodes List of statements to look for.
     * @return List of statement found.
     */
    public static List<Xnode> getNodes(Xnode from, Xnode to, List<Xcode> nodeOpCodes)
    {
        List<Xnode> unsupportedStatements = new ArrayList<>();
        Xnode crt = from;
        while (crt != null && crt.element() != to.element())
        {
            if (nodeOpCodes.contains(crt.opcode()))
            {
                unsupportedStatements.add(crt);
            }
            // Get all nodes matching in the subtree
            unsupportedStatements.addAll(getNodes(crt, nodeOpCodes));
            crt = crt.nextSibling();
        }

        if (crt != null && crt.equals(to) && nodeOpCodes.contains(crt.opcode()))
        {
            unsupportedStatements.add(crt);
        }
        return unsupportedStatements;
    }

    /* XNODE SECTION */

    /**
     * Delete all the elements between the two given elements.
     *
     * @param start The start element. Deletion start from next element.
     * @param end   The end element. Deletion end just before this element.
     */
    public static void deleteBetween(Xnode start, Xnode end)
    {
        List<Xnode> toDelete = new ArrayList<>();
        Xnode node = start.nextSibling();
        while (node != null && node.element() != end.element())
        {
            toDelete.add(node);
            node = node.nextSibling();
        }

        for (Xnode n : toDelete)
        {
            n.delete();
        }
    }

    /**
     * Get a list of T elements from an xpath query executed from the given element.
     *
     * @param from  Element to start from.
     * @param query XPath query to be executed.
     * @return List of all array references found. List is empty if nothing is
     *         found.
     */
    private static List<Xnode> getFromXpath(Xnode from, String query)
    {
        List<Xnode> elements = new ArrayList<>();
        try
        {
            XPathExpression ex = XPathFactory.newInstance().newXPath().compile(query);
            NodeList output = (NodeList) ex.evaluate(from.element(), XPathConstants.NODESET);
            for (int i = 0; i < output.getLength(); i++)
            {
                Element element = (Element) output.item(i);
                elements.add(new Xnode(element));
            }
        } catch (XPathExpressionException ignored)
        {
        }
        return elements;
    }

    /**
     * Read XML file.
     *
     * @param input Xml file path.
     * @return Document if the XML file could be read. Null otherwise.
     */
    public static Document readXmlFile(String input)
    {
        try
        {
            File fXmlFile = new File(input);
            if (!fXmlFile.exists())
            {
                return null;
            }
            DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
            Document doc = dBuilder.parse(fXmlFile);
            doc.getDocumentElement().normalize();
            return doc;
        } catch (Exception ignored)
        {
        }
        return null;
    }

    /**
     * Delete a node in the ast.
     *
     * @param node Node to be deleted.
     */
    public static void safeDelete(Xnode node)
    {
        if (node != null)
        {
            node.delete();
        }
    }

    /**
     * Gather string representation of the return value if there is one.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @param fctCall Function call to retrieve the return value.
     * @return String representation of the return value if any. Null otherwise.
     */
    public static String gatherReturnValue(XcodeProgram xcodeml, Xnode fctCall)
    {
        if (!Xnode.isOfCode(fctCall, Xcode.FUNCTION_CALL))
        {
            return null;
        }

        Xnode fctCallAncestor = fctCall.matchAncestor(Xcode.F_ASSIGN_STATEMENT);
        if (fctCallAncestor == null)
        {
            return null;
        }

        Xnode returnNode = fctCallAncestor.firstChild();
        if (xcodeml.getTypeTable().isBasicType(returnNode) && xcodeml.getTypeTable().getBasicType(returnNode).isArray())
        {
            return returnNode.constructRepresentation(false, false);
        }
        return null;
    }

    public static Set<String> getAllVariables(Xnode begin, Xnode end)
    {
        // Locate all declarations in the model-data block
        List<Xnode> decls = XnodeUtil.getNodes(begin, end, Collections.singletonList(Xcode.VAR_DECL));

        return decls.stream().map(x -> x.matchSeq(Xcode.NAME)).map(Xnode::value).collect(Collectors.toSet());
    }

    /**
     * Gather all nodes at the same level between from and to.
     *
     * @param from Node from which the block starts.
     * @param to   Node to which the block ends.
     * @return List of nodes in the block.
     */
    private static List<Xnode> getSiblingsBetween(Xnode from, Xnode to)
    {
        List<Xnode> siblingsInRegion = new LinkedList<>();
        Xnode current = from.nextSibling();
        while (current != null && !current.equals(to))
        {
            siblingsInRegion.add(current);
            current = current.nextSibling();
        }
        return siblingsInRegion;
    }

    /**
     * Gather all array identifiers written in the given block.
     *
     * @param from Node from which the block starts.
     * @param to   Node to which the block ends.
     * @return List of array identifiers written to in the block.
     */
    public static List<String> getWrittenArraysInRegion(Xnode from, Xnode to)
    {
        Set<String> writtenArraysIds = new HashSet<>();
        List<Xnode> firstLevelNodesInRegion;
        if (to == null)
        {
            firstLevelNodesInRegion = Collections.singletonList(from.nextSibling());
        } else
        {
            firstLevelNodesInRegion = getSiblingsBetween(from, to);
        }
        for (Xnode node : firstLevelNodesInRegion)
        {
            List<AssignStatement> assignements;
            if (node.is(Xcode.F_ASSIGN_STATEMENT))
            {
                assignements = Collections.singletonList(new AssignStatement(node.element()));
            } else
            {
                assignements = node.matchAll(Xcode.F_ASSIGN_STATEMENT).stream().map(Xnode::element)
                        .map(AssignStatement::new).collect(Collectors.toList());
            }
            for (AssignStatement as : assignements)
            {
                Xnode lhs = as.getLhs();
                if (lhs.is(Xcode.F_ARRAY_REF))
                {
                    writtenArraysIds.add(lhs.constructRepresentation(false, false));
                }
            }
        }
        return new ArrayList<>(writtenArraysIds);
    }

    /**
     * Gather all array identifiers read in the given block.
     *
     * @param from Node from which the block starts.
     * @param to   Node to which the block ends.
     * @return List of array identifiers read to in the block.
     */
    public static List<String> getReadArraysInRegion(Xnode from, Xnode to)
    {
        Set<String> readArrayIds = new HashSet<>();
        List<Xnode> firstLevelNodesInRegion;
        if (to == null)
        {
            firstLevelNodesInRegion = Collections.singletonList(from.nextSibling());
        } else
        {
            firstLevelNodesInRegion = getSiblingsBetween(from, to);
        }
        for (Xnode node : firstLevelNodesInRegion)
        {
            List<Xnode> arrayRefs = node.matchAll(Xcode.F_ARRAY_REF);
            for (Xnode arrayRef : arrayRefs)
            {
                if (arrayRef.ancestorIs(Xcode.F_ASSIGN_STATEMENT) && arrayRef.ancestor().firstChild().equals(arrayRef)
                        || arrayRef.matchAncestor(Xcode.F_ARRAY_REF) != null)
                {
                    continue;
                }

                if (arrayRef.matchAncestor(Xcode.F_MEMBER_REF) != null)
                {
                    readArrayIds.add(arrayRef.matchAncestor(Xcode.F_MEMBER_REF).constructRepresentation(false, false));
                } else
                {
                    readArrayIds.add(arrayRef.constructRepresentation(false, false));
                }
            }
        }
        return new ArrayList<>(readArrayIds);
    }
}
