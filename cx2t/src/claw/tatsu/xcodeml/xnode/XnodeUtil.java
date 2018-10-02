/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode;

import claw.tatsu.xcodeml.abstraction.HoistedNestedDoStatement;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
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

/**
 * The class XnodeUtil contains only static method to help manipulating the
 * raw Elements in the XcodeML representation by using the abstracted Xnode.
 *
 * @author clementval
 */

public class XnodeUtil {

  private XnodeUtil() {
    // Hide implicit public ctor
  }

  /**
   * Find all array references elements in a given body and give var name.
   *
   * @param parent    The body element to search for the array references.
   * @param arrayName Name of the array for the array reference to be found.
   * @return A list of all array references found.
   */
  public static List<Xnode> getAllArrayReferences(Xnode parent,
                                                  String arrayName)
  {
    List<Xnode> references = new ArrayList<>();
    NodeList nList = parent.element().
        getElementsByTagName(Xname.F_ARRAY_REF);
    for(int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        Xnode ref = new Xnode((Element) n);
        Xnode var = ref.matchSeq(Xcode.VAR_REF, Xcode.VAR);
        if(var != null && var.value().equalsIgnoreCase(arrayName)) {
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
  public static List<Xnode> getAllVarReferences(Xnode parent, String varName) {
    List<Xnode> references = new ArrayList<>();
    NodeList nList = parent.element().getElementsByTagName(Xname.VAR);
    for(int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if(n.getNodeType() == Node.ELEMENT_NODE) {
        Xnode var = new Xnode((Element) n);
        if(var.value().equalsIgnoreCase(varName)) {
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
  public static List<Xnode> getIdxRangesFromArrayRef(Xnode arrayRef) {
    List<Xnode> ranges = new ArrayList<>();
    if(arrayRef.opcode() != Xcode.F_ARRAY_REF) {
      return ranges;
    }
    for(Xnode el : arrayRef.children()) {
      if(el.opcode() == Xcode.INDEX_RANGE) {
        ranges.add(el);
      }
    }
    return ranges;
  }

  /**
   * <pre>
   * Intersect two sets of elements in XPath 1.0
   *
   * This method use Xpath to select the correct nodes. Xpath 1.0 does not have
   * the intersect operator but only union. By using the Kaysian Method, we can
   * it is possible to express the intersection of two node sets.
   *
   *       $set1[count(.|$set2)=count($set2)]
   *
   * </pre>
   *
   * @param s1 First set of element.
   * @param s2 Second set of element.
   * @return Xpath query that performs the intersect operator between s1 and s2.
   */
  private static String xPathIntersect(String s1, String s2) {
    return String.format("%s[count(.|%s)=count(%s)]", s1, s2, s2);
  }

  /**
   * <pre>
   * Find all assignment statement from a node until the end pragma.
   *
   * We intersect all assign statements which are next siblings of
   * the "from" element with all the assign statements which are previous
   * siblings of the ending pragma.
   * </pre>
   *
   * @param from      The element from which the search is initiated.
   * @param endPragma Value of the end pragma. Search will be performed until
   *                  there.
   * @return A list of all assign statements found. List is empty if no
   * statements are found.
   */
  public static List<Xnode> getArrayAssignInBlock(Xnode from, String endPragma)
  {
    /* Define all the assign element with array refs which are next siblings of
     * the "from" element */
    String s1 = String.format(
        "following-sibling::%s[%s]",
        Xname.F_ASSIGN_STATEMENT,
        Xname.F_ARRAY_REF
    );
    /* Define all the assign element with array refs which are previous siblings
     * of the end pragma element */
    String s2 = String.format(
        "following-sibling::%s[text()=\"%s\"]/preceding-sibling::%s[%s]",
        Xname.F_PRAGMA_STMT,
        endPragma,
        Xname.F_ASSIGN_STATEMENT,
        Xname.F_ARRAY_REF
    );
    // Use the Kaysian method to express the intersect operator
    String intersect = XnodeUtil.xPathIntersect(s1, s2);
    return getFromXpath(from, intersect);
  }

  /**
   * Get all array references in the siblings of the given element.
   *
   * @param from       Element to start from.
   * @param identifier Array name value.
   * @return List of all array references found. List is empty if nothing is
   * found.
   */
  public static List<Xnode> getAllArrayReferencesInSiblings(Xnode from,
                                                            String identifier)
  {
    String s1 = String.format("following-sibling::*//%s[%s[%s[text()=\"%s\"]]]",
        Xname.F_ARRAY_REF,
        Xname.VAR_REF,
        Xname.VAR,
        identifier
    );
    return getFromXpath(from, s1);
  }

  /**
   * Get the first assignment statement for an array reference.
   *
   * @param from      Statement to look from.
   * @param arrayName Identifier of the array.
   * @return The assignment statement if found. Null otherwise.
   */
  public static Xnode getFirstArrayAssign(Xnode from, String arrayName) {
    String s1 = String.format(
        "following::%s[%s[%s[%s[text()=\"%s\"]] and position()=1]]",
        Xname.F_ASSIGN_STATEMENT,
        Xname.F_ARRAY_REF,
        Xname.VAR_REF,
        Xname.VAR,
        arrayName
    );

    try {
      NodeList output = evaluateXpath(from.element(), s1);
      if(output.getLength() == 0) {
        return null;
      }
      Element assign = (Element) output.item(0);
      return new Xnode(assign);
    } catch(XPathExpressionException ignored) {
      return null;
    }
  }

  /**
   * Find all the nested do statement groups following the inductions iterations
   * define in inductionVars and being located between the "from" element and
   * the end pragma.
   *
   * @param from          Element from which the search is started.
   * @param endPragma     End pragma that terminates the search block.
   * @param inductionVars Induction variables that define the nested group to
   *                      locates.
   * @return List of do statements elements (outer most loops for each nested
   * group) found between the "from" element and the end pragma.
   */
  public static List<HoistedNestedDoStatement> findDoStatementForHoisting(
      Xnode from, Xnode endPragma, List<String> inductionVars)
  {

    /* s1 is selecting all the nested do statement groups that meet the criteria
     * from the "from" element down to the end of the block. */
    String s1 = "following::";

    String dynamicPartS1 = "";
    for(int i = inductionVars.size() - 1; i >= 0; --i) {
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
      if(i == inductionVars.size() - 1) { // first iteration
        tempQuery = String.format("%s[%s[text()=\"%s\"]]",
            Xname.F_DO_STATEMENT,
            Xname.VAR,
            inductionVars.get(i));
      } else {
        tempQuery = String.format("%s[%s[text()=\"%s\"] and %s[%s]]",
            Xname.F_DO_STATEMENT,
            Xname.VAR,
            inductionVars.get(i),
            Xname.BODY,
            dynamicPartS1); // Including previously formed xpath query
      }
      dynamicPartS1 = tempQuery;
    }
    s1 += dynamicPartS1;
    List<HoistedNestedDoStatement> doStatements = new ArrayList<>();
    try {
      NodeList output = evaluateXpath(from.element(), s1);
      for(int i = 0; i < output.getLength(); i++) {
        Element el = (Element) output.item(i);
        Xnode doStmt = new Xnode(el);
        if(doStmt.lineNo() != 0 &&
            doStmt.lineNo() < endPragma.lineNo() &&
            doStmt.lineNo() > from.lineNo())
        {
          doStatements.add(new HoistedNestedDoStatement(doStmt,
              inductionVars.size()));
        }
      }
    } catch(XPathExpressionException ignored) {
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
  private static NodeList evaluateXpath(Element from, String xpath)
      throws XPathExpressionException
  {
    XPathExpression ex = XPathFactory.newInstance().newXPath().compile(xpath);
    return (NodeList) ex.evaluate(from, XPathConstants.NODESET);
  }

  /**
   * Find all array references in the next children that match the given
   * criteria.
   *
   * This methods use powerful Xpath expression to locate the correct nodes in
   * the AST
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
  public static List<Xnode> getAllArrayReferencesByOffsets(Xnode from,
                                                           String identifier,
                                                           List<Integer>
                                                               offsets)
  {
    StringBuilder offsetXpath = new StringBuilder();
    for(int i = 0; i < offsets.size(); ++i) {
      if(offsets.get(i) == 0) {
        offsetXpath.append(String.format("%s[position()=%s and %s]",
            Xname.ARRAY_INDEX,
            i + 1,
            Xname.VAR
        ));
      } else if(offsets.get(i) > 0) {
        offsetXpath.append(String.
            format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                Xname.ARRAY_INDEX,
                i + 1,
                Xname.MINUS_EXPR,
                Xname.VAR,
                Xname.F_INT_CONST,
                offsets.get(i)));
      } else {
        offsetXpath.append(String.
            format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                Xname.ARRAY_INDEX,
                i + 1,
                Xname.MINUS_EXPR,
                Xname.VAR,
                Xname.F_INT_CONST,
                Math.abs(offsets.get(i))));
      }
      if(i != offsets.size() - 1) {
        offsetXpath.append(" and ");
      }
    }

    // Start of the Xpath query
    String xpathQuery = String.format(".//%s[%s[%s[text()=\"%s\"]] and %s]",
        Xname.F_ARRAY_REF,
        Xname.VAR_REF,
        Xname.VAR,
        identifier,
        offsetXpath.toString()
    );

    return getFromXpath(from, xpathQuery);
  }

  /**
   * Find all the index elements (arrayIndex and indexRange) in an element.
   *
   * @param parent Root element to search from.
   * @return A list of all index ranges found.
   */
  public static List<Xnode> findIndexes(Xnode parent) {
    List<Xnode> indexRanges = new ArrayList<>();
    if(parent == null || parent.element() == null) {
      return indexRanges;
    }

    Node node = parent.element().getFirstChild();
    while(node != null) {
      if(node.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) node;
        if(element.getTagName().equals(Xname.ARRAY_INDEX)
            || element.getTagName().equals(Xname.INDEX_RANGE))
        {
          indexRanges.add(new Xnode(element));
        }
      }
      node = node.getNextSibling();
    }

    return indexRanges;
  }

  /**
   * Find all the var elements that are real references to a variable. Var
   * element nested in an arrayIndex element are excluded.
   *
   * @param parent Root element to search from.
   * @return A list of all var elements found.
   */
  public static List<Xnode> findAllReferences(Xnode parent) {
    List<Xnode> vars = parent.matchAll(Xcode.VAR);
    List<Xnode> realReferences = new ArrayList<>();
    for(Xnode var : vars) {
      if(!((Element) var.element().getParentNode()).getTagName().
          equals(Xcode.ARRAY_INDEX.code()))
      {
        realReferences.add(var);
      }
    }
    return realReferences;
  }

  /**
   * Find all Xnode.VAR inside the given node and return their value. From
   * the set are excluded the variables used as indexes for vectors.
   *
   * @param node The node from where the research will start.
   * @return A set contains the variables used inside the node.
   */
  public static Set<String> findChildrenVariables(Xnode node) {
    List<Xnode> varNodes = node.matchAll(Xcode.VAR);
    Set<String> vars = new HashSet<>();
    for(Xnode xnode : varNodes) {
      // Skip vector indexes
      if(xnode.ancestor().opcode() == Xcode.ARRAY_INDEX) {
        continue;
      }
      vars.add(xnode.value());
    }
    return vars;
  }

  /**
   * Get all the variables names from a list of var elements.
   *
   * @param nodes List containing var element.
   * @return A set of all variable's name.
   */
  public static Set<String> getNamesFromReferences(List<Xnode> nodes) {
    Set<String> names = new HashSet<>();
    for(Xnode node : nodes) {
      if(node.opcode() != Xcode.VAR) {
        continue;
      }
      names.add(node.value());
    }
    return names;
  }

  /**
   * Find all the var elements that are real references to a variable. Var
   * element nested in an arrayIndex element are excluded.
   *
   * @param parent Root element to search from.
   * @param id     Identifier of the var to be found.
   * @return A list of all var elements found.
   */
  public static List<Xnode> findAllReferences(Xnode parent, String id) {
    List<Xnode> vars = parent.matchAll(Xcode.VAR);
    List<Xnode> realReferences = new ArrayList<>();
    for(Xnode var : vars) {
      if(!((Element) var.element().getParentNode()).getTagName().
          equals(Xcode.ARRAY_INDEX.code())
          && var.value().equalsIgnoreCase(id))
      {
        realReferences.add(var);
      }
    }
    return realReferences;
  }

  /**
   * Check whether the index of an arrayIndex element is an induction variable
   * given in the list.
   *
   * @param arrayIndex         arrayIndex to be checked.
   * @param inductionVariables List of potential induction variables.
   * @return True if the arrayIndex uses a var from induction variables list.
   * False in any other cases.
   */
  public static boolean isInductionIndex(Xnode arrayIndex,
                                         List<String> inductionVariables)
  {
    if(arrayIndex == null || arrayIndex.opcode() != Xcode.ARRAY_INDEX
        || inductionVariables == null || inductionVariables.isEmpty())
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
  public static List<Xnode> getNodes(Xnode root, List<Xcode> nodeOpcodes) {
    List<Xnode> unsupportedStatements = new ArrayList<>();
    if(root == null) {
      return unsupportedStatements;
    }
    for(Xcode opcode : nodeOpcodes) {
      unsupportedStatements.addAll(root.matchAll(opcode));
    }
    return unsupportedStatements;
  }

  /**
   * Get given statements in between from and to included.
   *
   * @param from        Node from.
   * @param to          Node to.
   * @param nodeOpcodes List of statements to look for.
   * @return List of statement found.
   */
  public static List<Xnode> getNodes(Xnode from, Xnode to,
                                     List<Xcode> nodeOpcodes)
  {
    List<Xnode> unsupportedStatements = new ArrayList<>();
    Xnode crt = from;
    while(crt != null && crt.element() != to.element()) {
      if(nodeOpcodes.contains(crt.opcode())) {
        unsupportedStatements.add(crt);
      }
      // Get all nodes matching in the subtree
      unsupportedStatements.addAll(getNodes(crt, nodeOpcodes));
      crt = crt.nextSibling();
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
  public static void deleteBetween(Xnode start, Xnode end) {
    List<Xnode> toDelete = new ArrayList<>();
    Xnode node = start.nextSibling();
    while(node != null && node.element() != end.element()) {
      toDelete.add(node);
      node = node.nextSibling();
    }

    for(Xnode n : toDelete) {
      n.delete();
    }
  }

  /**
   * Get a list of T elements from an xpath query executed from the
   * given element.
   *
   * @param from  Element to start from.
   * @param query XPath query to be executed.
   * @return List of all array references found. List is empty if nothing is
   * found.
   */
  private static List<Xnode> getFromXpath(Xnode from, String query)
  {
    List<Xnode> elements = new ArrayList<>();
    try {
      XPathExpression ex = XPathFactory.newInstance().newXPath().compile(query);
      NodeList output = (NodeList) ex.evaluate(from.element(),
          XPathConstants.NODESET);
      for(int i = 0; i < output.getLength(); i++) {
        Element element = (Element) output.item(i);
        elements.add(new Xnode(element));
      }
    } catch(XPathExpressionException ignored) {
    }
    return elements;
  }

  /**
   * Read XML file.
   *
   * @param input Xml file path.
   * @return Document if the XML file could be read. Null otherwise.
   */
  public static Document readXmlFile(String input) {
    try {
      File fXmlFile = new File(input);
      if(!fXmlFile.exists()) {
        return null;
      }
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();
      return doc;
    } catch(Exception ignored) {
    }
    return null;
  }

  /**
   * Delete a node in the ast.
   *
   * @param node Node to be deleted.
   */
  public static void safeDelete(Xnode node) {
    if(node != null) {
      node.delete();
    }
  }

  /**
   * Gather arguments of a function call.
   *
   * @param xcodeml   Current XcodeML translation unit.
   * @param fctCall   functionCall node in which the arguments are retrieved.
   * @param intent    Intent to use for gathering.
   * @param arrayOnly If true, gather only arrays arguments.
   * @return List of arguments as their string representation.
   */
  public static List<String> gatherArguments(XcodeProgram xcodeml,
                                             Xnode fctCall, Intent intent,
                                             boolean arrayOnly)
  {
    List<String> gatheredArguments = new ArrayList<>();
    if(fctCall == null || fctCall.opcode() != Xcode.FUNCTION_CALL) {
      return gatheredArguments;
    }
    Xnode argumentsNode = fctCall.matchDescendant(Xcode.ARGUMENTS);
    if(argumentsNode == null) {
      return gatheredArguments;
    }

    // Retrieve function type to check intents and types of parameters
    FfunctionType fctType = xcodeml.getTypeTable().getFunctionType(fctCall);
    List<Xnode> parameters = fctType.getParameters();
    List<Xnode> arguments = argumentsNode.children();

    for(int i = 0; i < parameters.size(); ++i) {
      // TODO handle optional arguments, named value args
      Xnode parameter = parameters.get(i);
      Xnode arg = arguments.get(i);

      String rep = "";
      if(FortranType.isBuiltInType(arg.getType()) && !arrayOnly
          && xcodeml.getTypeTable().isBasicType(parameter))
      {
        FbasicType btParameter = xcodeml.getTypeTable().getBasicType(parameter);
        if(!intent.isCompatible(btParameter.getIntent())) {
          continue;
        }
        rep = arg.constructRepresentation(false);
      } else if(xcodeml.getTypeTable().isBasicType(parameter)
          && xcodeml.getTypeTable().isBasicType(arg))
      {
        FbasicType btParameter = xcodeml.getTypeTable().getBasicType(parameter);
        FbasicType btArg = xcodeml.getTypeTable().getBasicType(arg);
        if((arrayOnly && !btArg.isArray() && !btArg.isAllocatable())
            || !intent.isCompatible(btParameter.getIntent()))
        {
          continue;
        }
        rep = arg.constructRepresentation(false);
      }
      if(rep != null && !rep.isEmpty()) {
        gatheredArguments.add(rep);
      }
    }
    return gatheredArguments;
  }

  public static Set<String> getAllVariables(Xnode begin, Xnode end) {
    Set<String> values = new HashSet<>();

    // Locate all declarations in the model-data block
    List<Xnode> decls = XnodeUtil.getNodes(begin, end,
        Collections.singletonList(Xcode.VAR_DECL));

    // Save variables for SCA usage
    for(Xnode varDecl : decls) {
      Xnode name = varDecl.matchSeq(Xcode.NAME);
      values.add(name.value());
    }
    return values;
  }
}
