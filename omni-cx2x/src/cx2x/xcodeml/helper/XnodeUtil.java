/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.helper;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.exception.*;

import cx2x.xcodeml.xnode.*;
import exc.xcodeml.XcodeMLtools_Fmod;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import javax.xml.xpath.*;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * The class XnodeUtil contains only static method to help manipulating the
 * raw Elements in the XcodeML representation by using the abstracted Xnode.
 *
 * @author clementval
 */

public class XnodeUtil {

  public static final String XMOD_FILE_EXTENSION = ".xmod";

  /**
   * Find a function definition according to a function call.
   * @param xcodeml The XcodeML program to search in.
   * @param fctCall The function call used to find the function definition.
   * @return A function definition element if found. Null otherwise.
   */
  public static XfunctionDefinition findFunctionDefinition(XcodeProgram xcodeml,
                                                           Xnode fctCall)
  {
    if(xcodeml.getElement() == null){
      return null;
    }
    String name = fctCall.findNode(Xcode.NAME).getValue();
    NodeList nList = xcodeml.getElement().
        getElementsByTagName(Xname.F_FUNCTION_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        Xnode dummyFctDef = new Xnode((Element)fctDefNode);
        Xnode fctDefName = dummyFctDef.find(Xcode.NAME);
        if(name != null &&
            fctDefName.getValue().toLowerCase().equals(name.toLowerCase()))
        {
          return new XfunctionDefinition(dummyFctDef.getElement());
        }
      }
    }
    return null;
  }

  /**
   * Find a function definition in a module definition.
   * @param module Module definition in which we search for the function
   *               definition.
   * @param name   Name of the function to be found.
   * @return A function definition element if found. Null otherwise.
   */
  public static XfunctionDefinition findFunctionDefinitionInModule(
      XmoduleDefinition module, String name)
  {
    if(module.getElement() == null){
      return null;
    }
    NodeList nList = module.getElement().
        getElementsByTagName(Xname.F_FUNCTION_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        XfunctionDefinition fctDef = new XfunctionDefinition((Element)n);
        if(fctDef.getName().getValue().equals(name)){
          return fctDef;
        }
      }
    }
    return null;
  }

  /**
   * Find all array references elements in a given body and give var name.
   * @param parent    The body element to search for the array references.
   * @param arrayName Name of the array for the array reference to be found.
   * @return A list of all array references found.
   */
  public static List<Xnode> getAllArrayReferences(Xnode parent,
                                                  String arrayName)
  {
    List<Xnode> references = new ArrayList<>();
    NodeList nList = parent.getElement().
        getElementsByTagName(Xname.F_ARRAY_REF);
    for (int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Xnode ref = new Xnode((Element) n);
        Xnode var = ref.find(Xcode.VARREF, Xcode.VAR);
        if(var != null && var.getValue().toLowerCase().
            equals(arrayName.toLowerCase()))
        {
          references.add(ref);
        }
      }
    }
    return references;
  }

  /**
   * Demote an array reference to a var reference.
   * @param ref     The array reference to be modified.
   */
  public static void demoteToScalar(Xnode ref){
    Xnode var = ref.find(Xcode.VARREF, Xcode.VAR).cloneObject();
    insertAfter(ref, var);
    ref.delete();
  }

  /**
   * Demote an array reference to a reference with fewer dimensions.
   * @param ref            The array reference to be modified.
   * @param keptDimensions List of dimensions to be kept. Dimension index starts
   *                       at 1.
   */
  public static void demote(Xnode ref, List<Integer> keptDimensions){
    for(int i = 1; i < ref.getChildren().size(); ++i){
      if(!keptDimensions.contains(i)){
        ref.getChild(i).delete();
      }
    }
  }

  /**
   * Retrieve the index ranges of an array notation.
   * @param arrayRef The array reference statements to extract the ranges from.
   * @return A list if indexRanges elements.
   */
  public static List<Xnode> getIdxRangesFromArrayRef(Xnode arrayRef){
    List<Xnode> ranges = new ArrayList<>();
    if(arrayRef.Opcode() != Xcode.FARRAYREF){
      return ranges;
    }
    for(Xnode el : arrayRef.getChildren()){
      if(el.Opcode() == Xcode.INDEXRANGE){
        ranges.add(el);
      }
    }
    return ranges;
  }


  /**
   * Compare two list of indexRange.
   * @param list1 First list of indexRange.
   * @param list2 Second list of indexRange.
   * @return True if the indexRange at the same position in the two list are all
   * identical. False otherwise.
   */
  public static boolean compareIndexRanges(List<Xnode> list1,
                                           List<Xnode> list2)
  {
    if(list1.size() != list2.size()){
      return false;
    }

    for(int i = 0; i < list1.size(); ++i){
      if(!isIndexRangeIdentical(list1.get(i), list2.get(i), true)){
        return false;
      }
    }
    return true;
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
   * @param s1 First set of element.
   * @param s2 Second set of element.
   * @return Xpath query that performs the intersect operator between s1 and s2.
   */
  private static String xPathIntersect(String s1, String s2){
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
  public static List<Xnode> getArrayAssignInBlock(Xnode from, String endPragma){
    /* Define all the assign element with array refs which are next siblings of
     * the "from" element */
    String s1 = String.format(
        "following-sibling::%s[%s]",
        Xname.F_ASSIGN_STMT,
        Xname.F_ARRAY_REF
    );
    /* Define all the assign element with array refs which are previous siblings
     * of the end pragma element */
    String s2 = String.format(
        "following-sibling::%s[text()=\"%s\"]/preceding-sibling::%s[%s]",
        Xname.PRAGMA_STMT,
        endPragma,
        Xname.F_ASSIGN_STMT,
        Xname.F_ARRAY_REF
    );
    // Use the Kaysian method to express the intersect operator
    String intersect = XnodeUtil.xPathIntersect(s1, s2);
    return getFromXpath(from, intersect);
  }

  /**
   * Get all array references in the siblings of the given element.
   * @param from        Element to start from.
   * @param identifier  Array name value.
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
   * @param from      Statement to look from.
   * @param arrayName Identifier of the array.
   * @return The assignment statement if found. Null otherwise.
   */
  public static Xnode getFirstArrayAssign(Xnode from, String arrayName){
    String s1 = String.format(
        "following::%s[%s[%s[%s[text()=\"%s\"]] and position()=1]]",
        Xname.F_ASSIGN_STMT,
        Xname.F_ARRAY_REF,
        Xname.VAR_REF,
        Xname.VAR,
        arrayName
    );

    try {
      NodeList output = evaluateXpath(from.getElement(), s1);
      if(output.getLength() == 0){
        return null;
      }
      Element assign = (Element) output.item(0);
      return new Xnode(assign);
    } catch (XPathExpressionException ignored) {
      return null;
    }
  }

  /**
   * Find all the nested do statement groups following the inductions iterations
   * define in inductionVars and being located between the "from" element and
   * the end pragma.
   * @param from          Element from which the search is started.
   * @param endPragma     End pragma that terminates the search block.
   * @param inductionVars Induction variables that define the nested group to
   *                      locates.
   * @return List of do statements elements (outer most loops for each nested
   * group) found between the "from" element and the end pragma.
   */
  public static List<Xnode> findDoStatement(Xnode from, Xnode endPragma,
                                            List<String> inductionVars)
  {

    /* s1 is selecting all the nested do statement groups that meet the criteria
     * from the "from" element down to the end of the block. */
    String s1 = "following::";

    String dynamic_part_s1 = "";
    for(int i = inductionVars.size() - 1; i >= 0; --i){
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
            dynamic_part_s1); // Including previously formed xpath query
      }
      dynamic_part_s1 = tempQuery;
    }
    s1 = s1 + dynamic_part_s1;
    List<Xnode> doStatements = new ArrayList<>();
    try {
      NodeList output = evaluateXpath(from.getElement(), s1);
      for (int i = 0; i < output.getLength(); i++) {
        Element el = (Element) output.item(i);
        Xnode doStmt = new Xnode(el);
        if(doStmt.getLineNo() != 0 &&
            doStmt.getLineNo() < endPragma.getLineNo())
        {
          doStatements.add(doStmt);
        }
      }
    } catch (XPathExpressionException ignored) {
    }
    return doStatements;
  }

  /**
   * Evaluates an Xpath expression and return its result as a NodeList.
   * @param from  Element to start the evaluation.
   * @param xpath Xpath expression.
   * @return Result of evaluation as a NodeList.
   * @throws XPathExpressionException if evaluation fails.
   */
  private static NodeList evaluateXpath(Element from, String xpath)
      throws XPathExpressionException
  {
    XPathExpression ex = XPathFactory.newInstance().newXPath().compile(xpath);
    return (NodeList)ex.evaluate(from, XPathConstants.NODESET);
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
   * @param from        The element from which the search is initiated.
   * @param identifier  Identifier of the array.
   * @param offsets     List of offsets to be search for.
   * @return A list of all array references found.
   */
  public static List<Xnode> getAllArrayReferencesByOffsets(Xnode from,
      String identifier, List<Integer> offsets)
  {
    String offsetXpath = "";
    for (int i = 0; i < offsets.size(); ++i){
      if(offsets.get(i) == 0){
        offsetXpath +=
            String.format("%s[position()=%s and %s]",
                Xname.ARRAY_INDEX,
                i+1,
                Xname.VAR
            );
      } else if(offsets.get(i) > 0) {
        offsetXpath +=
            String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                Xname.ARRAY_INDEX,
                i+1,
                Xname.MINUS_EXPR,
                Xname.VAR,
                Xname.F_INT_CONST,
                offsets.get(i));
      } else {
        offsetXpath +=
            String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                Xname.ARRAY_INDEX,
                i+1,
                Xname.MINUS_EXPR,
                Xname.VAR,
                Xname.F_INT_CONST,
                Math.abs(offsets.get(i)));
      }
      if(i != offsets.size()-1){
        offsetXpath += " and ";
      }
    }

    // Start of the Xpath query
    String xpathQuery = String.format(".//%s[%s[%s[text()=\"%s\"]] and %s]",
        Xname.F_ARRAY_REF,
        Xname.VAR_REF,
        Xname.VAR,
        identifier,
        offsetXpath
    );

    return getFromXpath(from, xpathQuery);
  }

  /**
   * Find a pragma element in the previous nodes containing a given keyword.
   * @param from    Element to start from.
   * @param keyword Keyword to be found in the pragma.
   * @return The pragma if found. Null otherwise.
   */
  public static Xnode findPreviousPragma(Xnode from, String keyword) {
    if (from == null || from.getElement() == null) {
      return null;
    }
    Node prev = from.getElement().getPreviousSibling();
    Node parent = from.getElement();
    do {
      while (prev != null) {
        if (prev.getNodeType() == Node.ELEMENT_NODE) {
          Element element = (Element) prev;
          if (element.getTagName().equals(Xcode.FPRAGMASTATEMENT.code())
              && element.getTextContent().toLowerCase().
              contains(keyword.toLowerCase())) {
            return new Xnode(element);
          }
        }
        prev = prev.getPreviousSibling();
      }
      parent = parent.getParentNode();
      prev = parent;
    } while (parent != null);
    return null;
  }

  /**
   * Find all the index elements (arrayIndex and indexRange) in an element.
   * @param parent  Root element to search from.
   * @return A list of all index ranges found.
   */
  public static List<Xnode> findIndexes(Xnode parent){
    List<Xnode> indexRanges = new ArrayList<>();
    if(parent == null || parent.getElement() == null){
      return indexRanges;
    }

    Node node = parent.getElement().getFirstChild();
    while (node != null){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        switch (element.getTagName()){
          case Xname.ARRAY_INDEX:
          case Xname.INDEX_RANGE:
            indexRanges.add(new Xnode(element));
            break;
        }
      }
      node = node.getNextSibling();
    }

    return indexRanges;
  }

  /**
   * Find all the name elements in an element.
   * @param parent Root element to search from.
   * @return A list of all name elements found.
   */
  public static List<Xnode> findAllNames(Xnode parent){
    return findAll(Xcode.NAME, parent);
  }


  /**
   * Find all the var elements that are real references to a variable. Var
   * element nested in an arrayIndex element are excluded.
   * @param parent Root element to search from.
   * @return A list of all var elements found.
   */
  public static List<Xnode> findAllReferences(Xnode parent){
    List<Xnode> vars = findAll(Xcode.VAR, parent);
    List<Xnode> realReferences = new ArrayList<>();
    for(Xnode var : vars){
      if(!((Element)var.getElement().getParentNode()).getTagName().
          equals(Xcode.ARRAYINDEX.code()))
      {
        realReferences.add(var);
      }
    }
    return realReferences;
  }

  /**
   * Find all the var elements that are real references to a variable. Var
   * element nested in an arrayIndex element are excluded.
   * @param parent Root element to search from.
   * @param id     Identifier of the var to be found.
   * @return A list of all var elements found.
   */
  public static List<Xnode> findAllReferences(Xnode parent, String id){
    List<Xnode> vars = findAll(Xcode.VAR, parent);
    List<Xnode> realReferences = new ArrayList<>();
    for(Xnode var : vars){
      if(!((Element)var.getElement().getParentNode()).getTagName().
          equals(Xcode.ARRAYINDEX.code())
          && var.getValue().toLowerCase().equals(id.toLowerCase()))
      {
        realReferences.add(var);
      }
    }
    return realReferences;
  }

  /**
   * Find all the pragma element in an XcodeML tree.
   * @param xcodeml The XcodeML program to search in.
   * @return A list of all pragmas found in the XcodeML program.
   */
  public static List<Xnode> findAllPragmas(XcodeProgram xcodeml){
    NodeList pragmaList = xcodeml.getDocument()
      .getElementsByTagName(Xcode.FPRAGMASTATEMENT.code());
    List<Xnode> pragmas = new ArrayList<>();
    for (int i = 0; i < pragmaList.getLength(); i++) {
      Node pragmaNode = pragmaList.item(i);
      if (pragmaNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) pragmaNode;
        pragmas.add(new Xnode(element));
      }
    }
    return pragmas;
  }

  /**
   * Extract the body of a do statement and place it directly after it.
   * @param loop The do statement containing the body to be extracted.
   */
  public static void extractBody(Xnode loop){
    extractBody(loop, loop);
  }

  /**
   * Extract the body of a do statement and place it after the reference node.
   * @param loop The do statement containing the body to be extracted.
   * @param ref  Element after which statement are shifted.
   */
  public static void extractBody(Xnode loop, Xnode ref){
    Element loopElement = loop.getElement();
    Element body = XnodeUtil.findFirstElement(loopElement,
        Xname.BODY);
    if(body == null){
      return;
    }
    Node refNode = ref.getElement();
    for(Node childNode = body.getFirstChild(); childNode!=null;){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        insertAfter(refNode, childNode);
        refNode = childNode;
      }
      childNode = nextChild;
    }
  }

  /**
   * Delete an element for the tree.
   * @param element Element to be deleted.
   */
  public static void delete(Element element){
    if(element == null || element.getParentNode() == null){
      return;
    }
    element.getParentNode().removeChild(element);
  }


  /**
   * Write the XcodeML to file or std out
   * @param xcodeml    The XcodeML to write in the output
   * @param outputFile Path of the output file or null to output on std out
   * @param indent     Number of spaces used for the indentation
   * @throws IllegalTransformationException if XML file cannot be written.
   */
  public static void writeXcodeML(XcodeML xcodeml, String outputFile,
                                     int indent)
    throws IllegalTransformationException
  {
    try {
      XnodeUtil.cleanEmptyTextNodes(xcodeml.getDocument());
      Transformer transformer
        = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount",
                Integer.toString(indent));
      DOMSource source = new DOMSource(xcodeml.getDocument());
      if(outputFile == null){
        // Output to console
        StreamResult console = new StreamResult(System.out);
        transformer.transform(source, console);
      } else {
        // Output to file
        StreamResult console = new StreamResult(new File(outputFile));
        transformer.transform(source, console);
      }
    } catch (Exception ignored){
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
   * @param parentNode Root node to start the cleaning.
   */
  private static void cleanEmptyTextNodes(Node parentNode) {
    boolean removeEmptyTextNodes = false;
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      removeEmptyTextNodes |= checkNodeTypes(childNode);
      childNode = childNode.getNextSibling();
    }

    if (removeEmptyTextNodes) {
      removeEmptyTextNodes(parentNode);
    }
  }

  /*
   * PRIVATE SECTION
   */

  /**
   * Remove all empty text nodes in the subtree.
   * @param parentNode Root node to start the search.
   */
  private static void removeEmptyTextNodes(Node parentNode) {
    Node childNode = parentNode.getFirstChild();
    while (childNode != null) {
      // grab the "nextSibling" before the child node is removed
      Node nextChild = childNode.getNextSibling();
      short nodeType = childNode.getNodeType();
      if (nodeType == Node.TEXT_NODE) {
        boolean containsOnlyWhitespace = childNode.getNodeValue()
          .trim().isEmpty();
        if (containsOnlyWhitespace) {
          parentNode.removeChild(childNode);
        }
      }
      childNode = nextChild;
    }
  }

  /**
   * Check the type of the given node.
   * @param childNode Node to be checked.
   * @return True if the node contains data. False otherwise.
   */
  private static boolean checkNodeTypes(Node childNode) {
    short nodeType = childNode.getNodeType();
    if (nodeType == Node.ELEMENT_NODE) {
      cleanEmptyTextNodes(childNode); // recurse into subtree
    }
    return nodeType == Node.ELEMENT_NODE || nodeType == Node.CDATA_SECTION_NODE
        || nodeType == Node.COMMENT_NODE;
  }

  /**
   * Insert a node directly after a reference node.
   * @param refNode The reference node. New node will be inserted after this
   *                one.
   * @param newNode The new node to be inserted.
   */
  private static void insertAfter(Node refNode, Node newNode){
     refNode.getParentNode().insertBefore(newNode, refNode.getNextSibling());
  }

  /**
   * Find the first element with tag corresponding to elementName nested under
   * the parent element.
   * @param parent      The root element to search from.
   * @param elementName The tag of the element to search for.
   * @return The first element found under parent with the corresponding tag.
   * Null if no element is found.
   */
  private static Element findFirstElement(Element parent, String elementName){
    NodeList elements = parent.getElementsByTagName(elementName);
    if(elements.getLength() == 0){
      return null;
    }
    return (Element) elements.item(0);
  }

  /**
   * Find the first element with tag corresponding to elementName in the direct
   * children of the parent element.
   * @param parent      The root element to search from.
   * @param elementName The tag of the element to search for.
   * @return The first element found in the direct children of the element
   * parent with the corresponding tag. Null if no element is found.
   */
  private static Element findFirstChildElement(Element parent, String elementName){
    NodeList nodeList = parent.getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(elementName)){
          return element;
        }
      }
    }
    return null;
  }

  /**
   * Get the depth of an element in the AST.
   * @param element XML element for which the depth is computed.
   * @return A depth value greater or equal to 0.
   */
  private static int getDepth(Element element){
    Node parent = element.getParentNode();
    int depth = 0;
    while(parent != null && parent.getNodeType() == Node.ELEMENT_NODE) {
      ++depth;
      parent = parent.getParentNode();
    }
    return depth;
  }

  /**
   * Shift all statements from the first siblings of the "from" element until
   * the "until" element (not included).
   * @param from       Start element for the swifting.
   * @param until      End element for the swifting.
   * @param targetBody Body element in which statements are inserted.
   */
  public static void shiftStatementsInBody(Xnode from,
                                           Xnode until, Xnode targetBody)
  {
    Node currentSibling = from.getElement().getNextSibling();
    Node firstStatementInBody = targetBody.getElement().getFirstChild();
    while(currentSibling != null && currentSibling != until.getElement()){
      Node nextSibling = currentSibling.getNextSibling();
      targetBody.getElement().insertBefore(currentSibling,
          firstStatementInBody);
      currentSibling = nextSibling;
    }
  }

  /**
   * Copy the whole body element into the destination one. Destination is
   * overwritten.
   * @param from The body to be copied.
   * @param to   The destination of the copied body.
   */
  public static void copyBody(Xnode from, Xnode to){
    Node copiedBody = from.cloneNode();
    if(to.getBody() != null){
      to.getBody().delete();
    }
    to.getElement().appendChild(copiedBody);
  }

  /**
   * Check whether the given type is a built-in type or is a type defined in the
   * type table.
   * @param type Type to check.
   * @return True if the type is built-in. False otherwise.
   */
  public static boolean isBuiltInType(String type){
    switch (type){
      case Xname.TYPE_F_CHAR:
      case Xname.TYPE_F_COMPLEX:
      case Xname.TYPE_F_INT:
      case Xname.TYPE_F_LOGICAL:
      case Xname.TYPE_F_REAL:
      case Xname.TYPE_F_VOID:
        return true;
      default:
        return false;
    }
  }

  /* XNODE SECTION */

  /**
   * Find module definition element in which the child is included if any.
   * @param from The child element to search from.
   * @return A XmoduleDefinition object if found. Null otherwise.
   */
  public static XmoduleDefinition findParentModule(Xnode from) {
    Xnode moduleDef = findParent(Xcode.FMODULEDEFINITION, from);
    if(moduleDef == null){
      return null;
    }
    return new XmoduleDefinition(moduleDef.getElement());
  }

  /**
   * Find function definition in the ancestor of the give element.
   * @param from Element to start search from.
   * @return The function definition found. Null if nothing found.
   */
  public static XfunctionDefinition findParentFunction(Xnode from){
    Xnode fctDef = findParent(Xcode.FFUNCTIONDEFINITION, from);
    if(fctDef == null){
      return null;
    }
    return new XfunctionDefinition(fctDef.getElement());
  }

  /**
   * Find element of the the given type that is directly after the given from
   * element.
   * @param opcode Code of the element to be found.
   * @param from   Element to start the search from.
   * @return The element found. Null if no element is found.
   */
  public static Xnode findDirectNext(Xcode opcode, Xnode from) {
    if(from == null){
      return null;
    }
    Node nextNode = from.getElement().getNextSibling();
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(opcode.code())){
          return new Xnode(element);
        }
        return null;
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  /**
   * Delete all the elements between the two given elements.
   * @param start The start element. Deletion start from next element.
   * @param end   The end element. Deletion end just before this element.
   */
  public static void deleteBetween(Xnode start, Xnode end){
    List<Element> toDelete = new ArrayList<>();
    Node node = start.getElement().getNextSibling();
    while (node != null && node != end.getElement()){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        toDelete.add(element);
      }
      node = node.getNextSibling();
    }

    for(Element e : toDelete){
      delete(e);
    }
  }

  /**
   * Insert an element just after a reference element.
   * @param refElement The reference element.
   * @param element    The element to be inserted.
   */
  public static void insertAfter(Xnode refElement, Xnode element){
    XnodeUtil.insertAfter(refElement.getElement(), element.getElement());
  }

  /**
   * Find the first element with tag corresponding to elementName.
   * @param opcode The XcodeML code of the element to search for.
   * @param parent The root element to search from.
   * @param any    If true, find in any nested element under parent. If false,
   *               only direct children are search for.
   * @return first element found. Null if no element is found.
   */
  public static Xnode find(Xcode opcode, Xnode parent, boolean any){
    Element el;
    if(any){
      el = findFirstElement(parent.getElement(), opcode.code());
    } else {
      el = findFirstChildElement(parent.getElement(), opcode.code());
    }
    return (el == null) ? null : new Xnode(el);
  }

  /**
   * Find element of the the given type that is directly after the given from
   * element.
   * @param opcode Code of the element to be found.
   * @param from   Element to start the search from.
   * @return The element found. Null if no element is found.
   */
  public static Xnode findNext(Xcode opcode, Xnode from) {
    return findInDirection(opcode, from, true);
  }

  /**
   * Find an element in the ancestor of the given element.
   * @param opcode Code of the element to be found.
   * @param from   Element to start the search from.
   * @return The element found. Null if nothing found.
   */
  public static Xnode findParent(Xcode opcode, Xnode from){
    return findInDirection(opcode, from, false);
  }

  /**
   * Find an element either in the next siblings or in the ancestors.
   * @param opcode Code of the element to be found.
   * @param from   Element to start the search from.
   * @param down   If True, search in the siblings. If false, search in the
   *               ancestors.
   * @return The element found. Null if nothing found.
   */
  private static Xnode findInDirection(Xcode opcode, Xnode from, boolean down){
    if(from == null){
      return null;
    }

    Node nextNode = down ? from.getElement().getNextSibling() :
        from.getElement().getParentNode();

    while(nextNode != null){
      if (nextNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) nextNode;
        if(element.getTagName().equals(opcode.code())){
          return new Xnode(element);
        }
      }
      nextNode = down ? nextNode.getNextSibling() : nextNode.getParentNode();
    }
    return null;
  }

  /**
   * Insert all the statements from a given body at the end of another body
   * @param originalBody The body in which the extra body will be appended
   * @param extraBody    The body that will be appended to the original body
   * @throws IllegalTransformationException if one of the body or their base
   *         element is null.
   */
  public static void appendBody(Xnode originalBody, Xnode extraBody)
      throws IllegalTransformationException
  {
    if(originalBody == null || originalBody.getElement() == null
        || extraBody == null || extraBody.getElement() == null
        || originalBody.Opcode() != Xcode.BODY
        || extraBody.Opcode() != Xcode.BODY)
    {
      throw new IllegalTransformationException("One of the body is null.");
    }

    // Append content of loop-body (loop) to this loop-body
    Node childNode = extraBody.getElement().getFirstChild();
    while(childNode != null){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        originalBody.getElement().appendChild(childNode);
      }
      childNode = nextChild;
    }
  }

  /**
   * Check if the two element are direct children of the same parent element.
   * @param e1 First element.
   * @param e2 Second element.
   * @return True if the two element are direct children of the same parent.
   * False otherwise.
   */
  public static boolean hasSameParentBlock(Xnode e1, Xnode e2) {
    return !(e1 == null || e2 == null || e1.getElement() == null
        || e2.getElement() == null)
        && e1.getElement().getParentNode() ==
        e2.getElement().getParentNode();
  }

  /**
   Compare the iteration range of two do statements.
   * @param e1             First do statement.
   * @param e2             Second do statement.
   * @param withLowerBound Compare lower bound or not.
   * @return True if the iteration range are identical.
   */
  private static boolean compareIndexRanges(Xnode e1, Xnode e2,
                                            boolean withLowerBound)
  {
    // The two nodes must be do statement
    if (e1.Opcode() != Xcode.FDOSTATEMENT || e2.Opcode() != Xcode.FDOSTATEMENT) {
      return false;
    }

    Xnode inductionVar1 = XnodeUtil.find(Xcode.VAR, e1, false);
    Xnode inductionVar2 = XnodeUtil.find(Xcode.VAR, e2, false);
    Xnode indexRange1 = XnodeUtil.find(Xcode.INDEXRANGE, e1, false);
    Xnode indexRange2 = XnodeUtil.find(Xcode.INDEXRANGE, e2, false);

    return compareValues(inductionVar1, inductionVar2) &&
        isIndexRangeIdentical(indexRange1, indexRange2, withLowerBound);
  }

  /**
   * Compare the iteration range of two do statements.
   * @param e1 First do statement.
   * @param e2 Second do statement.
   * @return True if the iteration range are identical.
   */
  public static boolean hasSameIndexRange(Xnode e1, Xnode e2) {
    return compareIndexRanges(e1, e2, true);
  }

  /**
   * Compare the iteration range of two do statements.
   * @param e1 First do statement.
   * @param e2 Second do statement.
   * @return True if the iteration range are identical besides the lower bound.
   */
  public static boolean hasSameIndexRangeBesidesLower(Xnode e1, Xnode e2) {
    return compareIndexRanges(e1, e2, false);
  }

  /**
   * Compare the inner values of two nodes.
   * @param n1 First node.
   * @param n2 Second node.
   * @return True if the values are identical. False otherwise.
   */
  private static boolean compareValues(Xnode n1, Xnode n2) {
    return !(n1 == null || n2 == null)
        && n1.getValue().toLowerCase().equals(n2.getValue().toLowerCase());
  }

  /**
   * Compare the inner value of the first child of two nodes.
   * @param n1 First node.
   * @param n2 Second node.
   * @return True if the value are identical. False otherwise.
   */
  private static boolean compareFirstChildValues(Xnode n1, Xnode n2) {
    if(n1 == null || n2 == null){
      return false;
    }
    Xnode c1 = n1.getChild(0);
    Xnode c2 = n2.getChild(0);
    return compareValues(c1, c2);
  }

  /**
   * Compare the inner values of two optional nodes.
   * @param n1 First node.
   * @param n2 Second node.
   * @return True if the values are identical or elements are null. False
   * otherwise.
   */
  private static boolean compareOptionalValues(Xnode n1, Xnode n2){
    return n1 == null && n2 == null || (n1 != null && n2 != null &&
        n1.getValue().toLowerCase().equals(n2.getValue().toLowerCase()));
  }

  /**
   * Compare the iteration range of two do statements
   * @param idx1           First do statement.
   * @param idx2           Second do statement.
   * @param withLowerBound If true, compare lower bound. If false, lower bound
   *                       is not compared.
   * @return True if the index range are identical.
   */
  private static boolean isIndexRangeIdentical(Xnode idx1, Xnode idx2,
                                               boolean withLowerBound)
  {
    if (idx1.Opcode() != Xcode.INDEXRANGE || idx2.Opcode() != Xcode.INDEXRANGE) {
      return false;
    }

    if (idx1.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE) &&
        idx2.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)) {
      return true;
    }

    Xnode low1 = idx1.find(Xcode.LOWERBOUND);
    Xnode up1 = idx1.find(Xcode.UPPERBOUND);
    Xnode low2 = idx2.find(Xcode.LOWERBOUND);
    Xnode up2 = idx2.find(Xcode.UPPERBOUND);
    Xnode s1 = idx1.find(Xcode.STEP);
    Xnode s2 = idx2.find(Xcode.STEP);

    if (s1 != null) {
      s1 = s1.getChild(0);
    }
    if (s2 != null) {
      s2 = s2.getChild(0);
    }

    if(withLowerBound){
      return compareFirstChildValues(low1, low2) &&
          compareFirstChildValues(up1, up2) && compareOptionalValues(s1, s2);
    } else {
      return compareFirstChildValues(up1, up2) && compareOptionalValues(s1, s2);
    }
  }

  /**
   * Swap the iteration range information of two do statement.
   * @param e1 First do statement.
   * @param e2 Second do statement.
   * @throws IllegalTransformationException if necessary elements are missing
   * to apply the transformation.
   */
  public static void swapIterationRange(Xnode e1, Xnode e2)
    throws IllegalTransformationException
  {
    // The two nodes must be do statement
    if(e1.Opcode() != Xcode.FDOSTATEMENT || e2.Opcode() != Xcode.FDOSTATEMENT){
      return;
    }

    Xnode inductionVar1 = XnodeUtil.find(Xcode.VAR, e1, false);
    Xnode inductionVar2 = XnodeUtil.find(Xcode.VAR, e2, false);
    Xnode indexRange1 = XnodeUtil.find(Xcode.INDEXRANGE, e1, false);
    Xnode indexRange2 = XnodeUtil.find(Xcode.INDEXRANGE, e2, false);
    if(inductionVar1 == null || inductionVar2 == null ||
        indexRange1 == null || indexRange2 == null)
    {
      throw new IllegalTransformationException("Induction variable or index " +
          "range missing.");
    }

    Xnode low1 = indexRange1.find(Xcode.LOWERBOUND).getChild(0);
    Xnode up1 = indexRange1.find(Xcode.UPPERBOUND).getChild(0);
    Xnode s1 = indexRange1.find(Xcode.STEP).getChild(0);

    Xnode low2 = indexRange2.find(Xcode.LOWERBOUND).getChild(0);
    Xnode up2 = indexRange2.find(Xcode.UPPERBOUND).getChild(0);
    Xnode s2 = indexRange2.find(Xcode.STEP).getChild(0);

    // Set the range of loop2 to loop1
    XnodeUtil.insertAfter(inductionVar2, inductionVar1.cloneObject());
    XnodeUtil.insertAfter(low2, low1.cloneObject());
    XnodeUtil.insertAfter(up2, up1.cloneObject());
    XnodeUtil.insertAfter(s2, s1.cloneObject());

    XnodeUtil.insertAfter(inductionVar1, inductionVar2.cloneObject());
    XnodeUtil.insertAfter(low1, low2.cloneObject());
    XnodeUtil.insertAfter(up1, up2.cloneObject());
    XnodeUtil.insertAfter(s1, s2.cloneObject());

    inductionVar1.delete();
    inductionVar2.delete();
    low1.delete();
    up1.delete();
    s1.delete();
    low2.delete();
    up2.delete();
    s2.delete();
  }

  /**
   * Get the depth of an element in the AST.
   * @param element The element for which the depth is computed.
   * @return A depth value greater or equal to 0.
   */
  public static int getDepth(Xnode element) {
    if(element == null || element.getElement() == null){
      return -1;
    }
    return getDepth(element.getElement());
  }

  /**
   * Copy the enhanced information from an element to a target element.
   * Enhanced information include line number and original file name.
   * @param base    Base element to copy information from.
   * @param target  Target element to copy information to.
   */
  public static void copyEnhancedInfo(Xnode base, Xnode target) {
    target.setLine(base.getLineNo());
    target.setFile(base.getFile());
  }

  /**
   * Insert an element just before a reference element.
   * @param ref    The reference element.
   * @param insert The element to be inserted.
   */
  public static void insertBefore(Xnode ref, Xnode insert){
    ref.getElement().getParentNode().insertBefore(insert.getElement(),
        ref.getElement());
  }

  /**
   * Get a list of T elements from an xpath query executed from the
   * given element.
   * @param from          Element to start from.
   * @param query         XPath query to be executed.
   * @return List of all array references found. List is empty if nothing is
   * found.
   */
  private static List<Xnode> getFromXpath(Xnode from, String query)
  {
    List<Xnode> elements = new ArrayList<>();
    try {
      XPathExpression ex = XPathFactory.newInstance().newXPath().compile(query);
      NodeList output = (NodeList) ex.evaluate(from.getElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element element = (Element) output.item(i);
        elements.add(new Xnode(element));
      }
    } catch (XPathExpressionException ignored) {
    }
    return elements;
  }

  /**
   * Find specific argument in a function call.
   * @param value   Value of the argument to be found.
   * @param fctCall Function call to search from.
   * @return The argument if found. Null otherwise.
   */
  public static Xnode findArg(String value, Xnode fctCall){
    if(fctCall.Opcode() != Xcode.FUNCTIONCALL) {
      return null;
    }
    Xnode args = fctCall.find(Xcode.ARGUMENTS);
    if(args == null){
      return null;
    }
    for(Xnode arg : args.getChildren()){
      if(value.toLowerCase().equals(arg.getValue().toLowerCase())){
        return arg;
      }
    }
    return null;
  }

  /**
   * Find all elements of a given type in the subtree.
   * @param opcode Type of the element to be found.
   * @param parent Root of the subtree.
   * @return List of all elements found in the subtree.
   */
  public static List<Xnode> findAll(Xcode opcode, Xnode parent) {
    List<Xnode> elements = new ArrayList<>();
    if(parent == null) {
      return elements;
    }
    NodeList nodes = parent.getElement().getElementsByTagName(opcode.code());
    for (int i = 0; i < nodes.getLength(); i++) {
      Node n = nodes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        elements.add(new Xnode((Element)n));
      }
    }
    return elements;
  }

  /**
   * Create a new FunctionCall element with elements name and arguments as
   * children.
   * @param xcodeml    The current XcodeML program unit in which the elements
   *                   are created.
   * @param returnType Value of the type attribute for the functionCall element.
   * @param name       Value of the name element.
   * @param nameType   Value of the type attribute for the name element.
   * @return The newly created element.
   */
  public static Xnode createFctCall(XcodeProgram xcodeml, String returnType,
                                    String name, String nameType){
    Xnode fctCall = new Xnode(Xcode.FUNCTIONCALL, xcodeml);
    fctCall.setAttribute(Xattr.TYPE, returnType);
    Xnode fctName = new Xnode(Xcode.NAME, xcodeml);
    fctName.setValue(name);
    fctName.setAttribute(Xattr.TYPE, nameType);
    Xnode args = new Xnode(Xcode.ARGUMENTS, xcodeml);
    fctCall.appendToChildren(fctName, false);
    fctCall.appendToChildren(args, false);
    return fctCall;
  }

  /**
   * Create a new FarrayRef element with varRef element as a child with the
   * given Var element.
   * @param xcodeml The current XcodeML program unit in which the elements
   *                are created.
   * @param type    Value of the type attribute for the FarrayRef element.
   * @param var     Var element nested in the varRef element.
   * @return The newly created element.
   */
  public static Xnode createArrayRef(XcodeProgram xcodeml, XbasicType type,
                                     Xnode var)
  {
    Xnode ref = new Xnode(Xcode.FARRAYREF, xcodeml);
    ref.setAttribute(Xattr.TYPE, type.getRef());
    Xnode varRef = new Xnode(Xcode.VARREF, xcodeml);
    varRef.setAttribute(Xattr.TYPE, type.getType());
    varRef.appendToChildren(var, false);
    ref.appendToChildren(varRef, false);
    return ref;
  }

  /**
   * Create a new Id element with all the underlying needed elements.
   * @param xcodeml   The current XcodeML program unit in which the elements
   *                  are created.
   * @param type      Value for the attribute type.
   * @param sclass    Value for the attribute sclass.
   * @param nameValue Value of the name inner element.
   * @return The newly created element.
   */
  public static Xid createId(XcodeProgram xcodeml, String type, String sclass,
                             String nameValue)
  {
    Xnode id = new Xnode(Xcode.ID, xcodeml);
    Xnode internalName = new Xnode(Xcode.NAME, xcodeml);
    internalName.setValue(nameValue);
    id.appendToChildren(internalName, false);
    id.setAttribute(Xattr.TYPE, type);
    id.setAttribute(Xattr.SCLASS, sclass);
    return new Xid(id.getElement());
  }

  /**
   * Constructs a new basicType element with the given information.
   * @param xcodeml The current XcodeML file unit in which the elements
   *                are created.
   * @param type    Type hash.
   * @param ref     Reference type.
   * @param intent  Optional intent information.
   * @return The newly created element.
   */
  public static XbasicType createBasicType(XcodeML xcodeml, String type,
                                           String ref, Xintent intent)
  {
    Xnode bt = new Xnode(Xcode.FBASICTYPE, xcodeml);
    bt.setAttribute(Xattr.TYPE, type);
    if(ref != null) {
      bt.setAttribute(Xattr.REF, ref);
    }
    if(intent != null) {
      bt.setAttribute(Xattr.INTENT, intent.toString());
    }
    return new XbasicType(bt.getElement());
  }

  /**
   * Create a new Xdecl object with all the underlying elements for a varDecl.
   * @param xcodeml The current XcodeML file unit in which the elements
   *                are created.
   * @param nameType  Value for the attribute type of the name element.
   * @param nameValue Value of the name inner element.
   * @return The newly created element.
   */
  public static Xdecl createVarDecl(XcodeML xcodeml, String nameType,
                                       String nameValue)
  {
    Xnode varD = new Xnode(Xcode.VARDECL, xcodeml);
    Xnode internalName = new Xnode(Xcode.NAME, xcodeml);
    internalName.setValue(nameValue);
    internalName.setAttribute(Xattr.TYPE, nameType);
    varD.appendToChildren(internalName, false);
    return new Xdecl(varD.getElement());
  }

  /**
   * Constructs a new name element with name value and optional type.
   * @param xcodeml Current XcodeML file unit in which the element is
   *                created.
   * @param name    Name value.
   * @param type    Optional type value.
   * @return The newly created element.
   */
  private static Xnode createName(XcodeML xcodeml, String name, String type)
  {
    Xnode n = new Xnode(Xcode.NAME, xcodeml);
    n.setValue(name);
    if(type != null){
      n.setAttribute(Xattr.TYPE, type);
    }
    return n;
  }

  /**
   * Create an empty assumed shape indexRange element.
   * @param xcodeml Current XcodeML file unit in which the element is
   *                created.
   * @return The newly created element.
   */
  public static Xnode createEmptyAssumedShaped(XcodeML xcodeml) {
    Xnode range = new Xnode(Xcode.INDEXRANGE, xcodeml);
    range.setAttribute(Xattr.IS_ASSUMED_SHAPE, Xname.TRUE);
    return range;
  }

  /**
   * Create an indexRange element to loop over an assumed shape array.
   * @param xcodeml    Current XcodeML file unit in which the element is
   *                   created.
   * @param arrayVar   Var element representing the array variable.
   * @param startIndex Lower bound index value.
   * @param dimension  Dimension index for the upper bound value.
   * @return The newly created element.
   */
  public static Xnode createAssumedShapeRange(XcodeML xcodeml, Xnode arrayVar,
                                              int startIndex, int dimension)
  {
    // Base structure
    Xnode indexRange = new Xnode(Xcode.INDEXRANGE, xcodeml);
    Xnode lower = new Xnode(Xcode.LOWERBOUND, xcodeml);
    Xnode upper = new Xnode(Xcode.UPPERBOUND, xcodeml);
    indexRange.appendToChildren(lower, false);
    indexRange.appendToChildren(upper, false);

    // Lower bound
    Xnode lowerBound = new Xnode(Xcode.FINTCONSTANT, xcodeml);
    lowerBound.setValue(String.valueOf(startIndex));
    lower.appendToChildren(lowerBound, false);

    // Upper bound
    Xnode fctCall = new Xnode(Xcode.FUNCTIONCALL, xcodeml);
    upper.appendToChildren(fctCall, false);
    fctCall.setAttribute(Xattr.IS_INTRINSIC, Xname.TRUE);
    fctCall.setAttribute(Xattr.TYPE, Xname.TYPE_F_INT);
    Xnode name = new Xnode(Xcode.NAME, xcodeml);
    name.setValue(Xname.INTRINSIC_SIZE);
    fctCall.appendToChildren(name, false);
    Xnode args = new Xnode(Xcode.ARGUMENTS, xcodeml);
    fctCall.appendToChildren(args, false);
    args.appendToChildren(arrayVar, true);
    Xnode dim = new Xnode(Xcode.FINTCONSTANT, xcodeml);
    dim.setValue(String.valueOf(dimension));
    args.appendToChildren(dim, false);
    return indexRange;
  }

  /**
   * Create a new FifStatement element with an empty then body.
   * @param xcodeml    Current XcodeML file unit in which the element is
   *                   created.
   * @return The newly created element.
   */
  public static Xnode createIfThen(XcodeML xcodeml){
    Xnode root = new Xnode(Xcode.FIFSTATEMENT, xcodeml);
    Xnode cond = new Xnode(Xcode.CONDITION, xcodeml);
    Xnode thenBlock = new Xnode(Xcode.THEN, xcodeml);
    Xnode thenBody = new Xnode(Xcode.BODY, xcodeml);
    thenBlock.appendToChildren(thenBody, false);
    root.appendToChildren(cond, false);
    root.appendToChildren(thenBlock, false);
    return root;
  }

  /**
   * Create a new FdoStatement element with an empty body.
   * @param xcodeml      Current XcodeML file unit in which the element is
   *                     created.
   * @param inductionVar Var element for the induction variable.
   * @param indexRange   indexRange element for the iteration range.
   * @return The newly created element.
   */
  public static Xnode createDoStmt(XcodeML xcodeml, Xnode inductionVar,
                                   Xnode indexRange)
  {
    Xnode root = new Xnode(Xcode.FDOSTATEMENT, xcodeml);
    root.appendToChildren(inductionVar, false);
    root.appendToChildren(indexRange, false);
    Xnode body = new Xnode(Xcode.BODY, xcodeml);
    root.appendToChildren(body, false);
    return root;
  }

  /**
   * Create a new var element.
   * @param type    Value of the type attribute.
   * @param value   Value of the var.
   * @param scope   Value of the scope attribute.
   * @param xcodeml Current XcodeML file unit in which the element is created.
   * @return The newly created element.
   */
  public static Xnode createVar(String type, String value, Xscope scope,
                                XcodeML xcodeml)
  {
    Xnode var = new Xnode(Xcode.VAR, xcodeml);
    var.setAttribute(Xattr.TYPE, type);
    var.setAttribute(Xattr.SCOPE, scope.toString());
    var.setValue(value);
    return var;
  }

  /**
   * Create a new namedValue element with its attribute.
   * @param value   Value of the name attribute.
   * @param xcodeml Current XcodeML file unit in which the element is created.
   * @return The newly created element.
   */
  public static Xnode createNamedValue(String value, XcodeML xcodeml){
    Xnode namedValue = new Xnode(Xcode.NAMEDVALUE, xcodeml);
    namedValue.setAttribute(Xattr.NAME, value);
    return namedValue;
  }

  /**
   * Find module containing the function and read its .xmod file.
   * @param fctDef Function definition nested in the module.
   * @return Xmod object if the module has been found and read. Null otherwise.
   */
  private static Xmod findContainingModule(XfunctionDefinition fctDef){
    XmoduleDefinition mod = findParentModule(fctDef);
    if(mod == null){
      return null;
    }
    String modName = mod.getAttribute(Xattr.NAME);
    for(String dir : XcodeMLtools_Fmod.getSearchPath()){
      String path = dir + "/" + modName + XMOD_FILE_EXTENSION;
      File f = new File(path);
      if(f.exists()){
        Document doc = readXmlFile(path);
        return doc != null ? new Xmod(doc, modName, dir) : null;
      }
    }
    return null;
  }

  /**
   * Read XML file.
   * @param input Xml file path.
   * @return Document if the XML file could be read. Null otherwise.
   */
  public static Document readXmlFile(String input){
    try {
      File fXmlFile = new File(input);
      if(!fXmlFile.exists()){
        return null;
      }
      DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
      DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
      Document doc = dBuilder.parse(fXmlFile);
      doc.getDocumentElement().normalize();
      return doc;
    } catch(Exception ignored){}
    return null;
  }

  /**
   * Create the id and varDecl elements and add them to the symbol/declaration
   * table.
   * @param name    Name of the variable.
   * @param type    Type of the variable.
   * @param sclass  Scope class of the variable (from Xname).
   * @param fctDef  Function definition in which id and decl are created.
   * @param xcodeml Current XcodeML program unit in which the elements will be
   *                created.
   */
  public static void createIdAndDecl(String name, String type, String sclass,
                                     XfunctionDefinition fctDef,
                                     XcodeProgram xcodeml)
  {
    Xid id = XnodeUtil.createId(xcodeml, type, sclass, name);
    fctDef.getSymbolTable().add(id);
    Xdecl decl = XnodeUtil.createVarDecl(xcodeml, type, name);
    fctDef.getDeclarationTable().add(decl);
  }

  /**
   * Create a name element and adds it as a parameter of the given function
   * type.
   * @param xcodeml   Current XcodeML file unit.
   * @param nameValue Value of the name element to create.
   * @param type      Type of the name element to create.
   * @param fctType   Function type in which the element will be added as a
   *                  parameter.
   */
  public static void createAndAddParam(XcodeML xcodeml, String nameValue,
                                       String type, XfunctionType fctType)
  {
    Xnode param = XnodeUtil.createName(xcodeml, nameValue, type);
    fctType.getParams().add(param);
  }

  /**
   * Create a name element and adds it as a parameter of the given function
   * type if this parameter is does not exist yet.
   * @param xcodeml   Current XcodeML file unit.
   * @param nameValue Value of the name element to create.
   * @param type      Type of the name element to create.
   * @param fctType   Function type in which the element will be added as a
   *                  parameter.
   */
  public static void createAndAddParamIfNotExists(XcodeML xcodeml,
                                                  String nameValue, String type,
                                                  XfunctionType fctType)
  {
    for(Xnode p : fctType.getParams().getAll()){
      if(p.getValue().toLowerCase().equals(nameValue.toLowerCase())){
        return;
      }
    }
    Xnode param = XnodeUtil.createName(xcodeml, nameValue, type);
    fctType.getParams().add(param);
  }

  /**
   * Update the function signature in the module file to reflects local changes.
   * @param xcodeml     Current XcodeML file unit.
   * @param fctDef      Function definition that has been changed.
   * @param fctType     Function type that has been changed.
   * @param modDef      Module definition holding the function definition.
   * @param claw        Pragma that has triggered the transformation.
   * @param transformer Current transformer object.
   * @throws IllegalTransformationException If the module file or the function
   * cannot be located
   */
  public static void updateModuleSignature(XcodeProgram xcodeml,
                                           XfunctionDefinition fctDef,
                                           XfunctionType fctType,
                                           XmoduleDefinition modDef,
                                           ClawLanguage claw,
                                           cx2x.xcodeml.transformation.
                                               Transformer transformer)
      throws IllegalTransformationException
  {
    Xmod mod;
    if(transformer.getModCache().isModuleLoaded(modDef.getName())){
      mod = transformer.getModCache().get(modDef.getName());
    } else {
      mod = XnodeUtil.findContainingModule(fctDef);
      transformer.getModCache().add(modDef.getName(), mod);
      if(mod == null){
        throw new IllegalTransformationException(
            "Unable to locate module file for: " + modDef.getName(),
            claw.getPragma().getLineNo());
      }
    }


    XfunctionType fctTypeMod = (XfunctionType) mod.getTypeTable().get(
        fctDef.getName().getAttribute(Xattr.TYPE));

    if(fctTypeMod == null){
      /* Workaround for a bug in OMNI Compiler. Look at test case
       * claw/abstraction12. In this test case, the XcodeML/F intermediate
       * representation for the function call points to a FfunctionType element
       * with no parameters. Thus, we have to find the correct FfunctionType
       * for the same function/subroutine with the same name in the module
       * symbol table. */
      String errorMsg = "Unable to locate fct " + fctDef.getName().getValue() +
          " in module " + modDef.getName();
      int lineNo = claw.getPragma().getLineNo();

      // If not, try to find the correct FfunctionType in the module definitions
      Xid id = mod.getIdentifiers().get(fctDef.getName().getValue());
      if(id == null){
        throw new IllegalTransformationException(errorMsg, lineNo);
      }
      fctTypeMod = (XfunctionType)mod.getTypeTable().get(id.getType());
      if(fctTypeMod == null){
        throw new IllegalTransformationException(errorMsg, lineNo);
      }
    }

    XbasicType modIntTypeIntentIn = XnodeUtil.createBasicType(mod,
        mod.getTypeTable().generateIntegerTypeHash(),
        Xname.TYPE_F_INT, Xintent.IN);
    mod.getTypeTable().add(modIntTypeIntentIn);

    List<Xnode> paramsLocal = fctType.getParams().getAll();
    List<Xnode> paramsMod = fctTypeMod.getParams().getAll();


    if(paramsLocal.size() < paramsMod.size()){
      throw new IllegalTransformationException(
          "Local function has more parameters than module counterpart.",
          claw.getPragma().getLineNo());
    }

    for(int i = 0; i < paramsLocal.size(); i++){
      Xnode pLocal = paramsLocal.get(i);
      if(i > (paramsMod.size() - 1)) {
        // new parameter
        XnodeUtil.createAndAddParam(mod, pLocal.getValue(),
            modIntTypeIntentIn.getType(), fctTypeMod);
      } else {
        Xnode pMod = paramsMod.get(i);
        String localType = pLocal.getAttribute(Xattr.TYPE);
        String modType = pMod.getAttribute(Xattr.TYPE);
        if(!localType.equals(modType)){
          // Param has been update so have to replicate the change to mod file
          XbasicType lType = (XbasicType)xcodeml.getTypeTable().get(localType);
          XbasicType crtType = (XbasicType)mod.getTypeTable().get(modType);

          if(lType.isArray()) {
            String newType = duplicateWithDimension(lType, crtType, mod);
            pMod.setAttribute(Xattr.TYPE, newType);
          }
        }
      }
    }
  }

  /**
   * Duplicates the type to update and add extra dimensions to match the base
   * type.
   * @param base     Base type.
   * @param toUpdate Type to update.
   * @param xcodeml  Current XcodeML file unit.
   * @return The new type hash generated.
   */
  public static String duplicateWithDimension(XbasicType base,
                                              XbasicType toUpdate,
                                              XcodeML xcodeml)
  {
    XbasicType newType = toUpdate.cloneObject();
    String type = xcodeml.getTypeTable().generateArrayTypeHash();
    newType.setAttribute(Xattr.TYPE, type);

    int additionalDimensions = base.getDimensions() - toUpdate.getDimensions();
    for(int i = 0; i < additionalDimensions; ++i){
      Xnode index = XnodeUtil.createEmptyAssumedShaped(xcodeml);
      newType.addDimension(index, 0);
    }

    xcodeml.getTypeTable().add(newType);
    return type;
  }

  /**
   * Try to locate a function definition in the current declaration table or
   * recursively in the modules' delcaration tables.
   * @param dt      Declaration table to search in.
   * @param fctName Function's name to be found.
   * @return The function definition if found. Null otherwise.
   */
  public static XfunctionDefinition findFunctionDefinition(XglobalDeclTable dt,
                                                           String fctName)
  {
    Iterator<Map.Entry<String, Xnode>> it = dt.getIterator();
    while(it.hasNext()){
      Map.Entry<String, Xnode> entry = it.next();
      if(entry.getValue() instanceof XmoduleDefinition){
        XfunctionDefinition fctDef = findFunctionDefinitionInModule(
            (XmoduleDefinition)entry.getValue(), fctName);
        if(fctDef != null){
          return fctDef;
        }
      } else if (entry.getValue() instanceof XfunctionDefinition){
        XfunctionDefinition fctDef = (XfunctionDefinition)entry.getValue();
        if(fctDef.getName().getValue().equals(fctName)){
          return fctDef;
        }
      }
    }
    return null;
  }

  /**
   * Get next sibling node.
   * @param crt Current node.
   * @return Next sibling node.
   */
  public static Xnode getNextSibling(Xnode crt){
    Node n = crt.getElement().getNextSibling();
    while (n != null){
      if(n.getNodeType() == Node.ELEMENT_NODE){
        return new Xnode((Element)n);
      }
      n = n.getNextSibling();
    }
    return null;
  }

  /**
   * Get all the USE statement declaration in a module definition.
   * @param mod Module definition.
   * @return A list of all declaration. Empty list if no USE declaration.
   */
  public static List<Xdecl> getAllUse(XmoduleDefinition mod){
    return mod == null ? getAllUseFromDeclTable(null) :
        getAllUseFromDeclTable(mod.getDeclarationTable());
  }

  /**
   * Get all the USE statement declaration in a function definition.
   * @param fctDef Function definition.
   * @return A list of all declaration. Empty list if no USE declaration.
   */
  public static List<Xdecl> getAllUse(XfunctionDefinition fctDef){
    return fctDef == null ? getAllUseFromDeclTable(null) :
        getAllUseFromDeclTable(fctDef.getDeclarationTable());
  }

  /**
   * Get all the USE statement declaration in a declaration table.
   * @param dt Declaration table.
   * @return A list of all declaration. Empty list if no USE declaration.
   */
  private static List<Xdecl> getAllUseFromDeclTable(XdeclTable dt){
    if(dt == null){
      return new ArrayList<Xdecl>();
    }
    List<Xdecl> uses = dt.getAll(Xcode.FUSEDECL);
    uses.addAll(dt.getAll(Xcode.FUSEONLYDECL));
    return uses;
  }

}
