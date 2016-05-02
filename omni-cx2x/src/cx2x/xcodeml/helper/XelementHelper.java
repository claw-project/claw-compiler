/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.helper;

import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.xelement.*;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.NamedNodeMap;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import javax.xml.xpath.*;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * The class XelementHelper contains only static method to help manipulating the
 * raw Elements in the XcodeML representation by using the abstracted Xelements.
 *
 * @author clementval
 */

public class XelementHelper {


  /**
   * Get a text attribute value from an element.
   * @param el        The element in which the attribute is searched.
   * @param attrName  The name of the attribute to be found.
   * @return The attribute's value if the attribute is found. Null otherwise.
   */
  public static String getAttributeValue(XbaseElement el, String attrName){
    if(el == null || el.getBaseElement() == null){
      return null;
    }
    NamedNodeMap attributes = el.getBaseElement().getAttributes();
    for (int j = 0; j < attributes.getLength(); j++) {
      if(attributes.item(j).getNodeName().equals(attrName)){
        return attributes.item(j).getNodeValue();
      }
    }
    return null;
  }

  /**
   * Get a boolean attribute value from an element.
   * @param el        The element in which the attribute is searched.
   * @param attrName  The name of the attribute to be found.
   * @return The attribute's value if the attribute is found. Null otherwise.
   */
  public static boolean getBooleanAttributeValue(XbaseElement el,
    String attrName) {
    if (el == null || el.getBaseElement() == null) {
      return false;
    }
    String value = XelementHelper.getAttributeValue(el, attrName);
    return value != null && value.equals(XelementName.TRUE);
  }

  /**
   * Find a function definition according to a function call.
   * @param xcodeml The XcodeML program to search in.
   * @param fctCall The function call used to find the function definition.
   * @return A function definition element if found. Null otherwise.
   */
  public static XfunctionDefinition findFunctionDefinition(XcodeProgram xcodeml,
                                                           XfunctionCall fctCall)
  {
    if(xcodeml.getBaseElement() == null){
      return null;
    }
    String name = fctCall.getName().getValue();
    NodeList nList = xcodeml.getBaseElement().getElementsByTagName(XelementName.FCT_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        XbaseElement dummyFctDef = new XbaseElement((Element)fctDefNode);
        Xname fctDefName = findName(dummyFctDef, false);
        if(name != null && fctDefName.isIdentical(name)){
          return new XfunctionDefinition(dummyFctDef.getBaseElement());
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
    if(module.getBaseElement() == null){
      return null;
    }
    NodeList nList = module.getBaseElement().
        getElementsByTagName(XelementName.FCT_DEFINITION);
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
   * Find all array references elements in a given body.
   * @param parent The body element to search for the array references.
   * @return A list of all array references found.
   */
  public static List<XarrayRef> getAllArrayReferences(XbaseElement parent){
    List<XarrayRef> references = new ArrayList<>();
    NodeList nList = parent.getBaseElement().
      getElementsByTagName(XelementName.F_ARRAY_REF);
    for (int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XarrayRef ref = new XarrayRef(el);
        references.add(ref);
      }
    }
    return references;
  }

  /**
   * Retrieve the index ranges of an array notation.
   * @param arrayRef The array reference statements to extract the ranges from.
   * @return A list if indexRanges elements.
   */
  public static List<XindexRange> getIdxRangesFromArrayRef(XarrayRef arrayRef){
    List<XindexRange> ranges = new ArrayList<>();
    for(XbaseElement el : arrayRef.getInnerElements()){
      if(el instanceof XindexRange){
        ranges.add((XindexRange) el);
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
  public static boolean compareIndexRanges(List<XindexRange> list1,
                                           List<XindexRange> list2){
    if(list1.size() != list2.size()){
      return false;
    }

    for(int i = 0; i < list1.size(); ++i){
      if(!list1.get(i).equals(list2.get(i))){
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
   * We intersect all assign statments which are next siblings of
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
  public static List<XassignStatement> getArrayAssignInBlock(XbaseElement from,
                                                             String endPragma)
  {

    /* Define all the assign element with array refs which are next siblings of
     * the "from" element */
    String s1 = String.format(
        "following-sibling::%s[%s]",
        XelementName.F_ASSIGN_STMT,
        XelementName.F_ARRAY_REF
    );
    /* Define all the assign element with array refs which are previous siblings
     * of the end pragma element */
    String s2 = String.format(
        "following-sibling::%s[text()=\"%s\"]/preceding-sibling::%s[%s]",
        XelementName.PRAGMA_STMT,
        endPragma,
        XelementName.F_ASSIGN_STMT,
        XelementName.F_ARRAY_REF
    );
    // Use the Kaysian method to express the intersect operator
    String intersect = XelementHelper.xPathIntersect(s1, s2);

    List<XassignStatement> assignements = new ArrayList<>();
    try {
      XPathFactory xPathfactory = XPathFactory.newInstance();
      XPath xpath = xPathfactory.newXPath();
      XPathExpression xpathExpr = xpath.compile(intersect);
      NodeList output = (NodeList) xpathExpr.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element assign = (Element) output.item(i);
        assignements.add(new XassignStatement(assign));
      }
    } catch (XPathExpressionException ignored) {}
    return assignements;
  }

  /**
   * Get all array references in the siblings of the given element.
   * @param from        Element to start from.
   * @param identifier  Array name value.
   * @return List of all array references found. List is empty if nothing is
   * found.
   */
  public static List<XarrayRef> getAllArrayReferencesInSiblings(
      XbaseElement from,
      String identifier)
  {
    String s1 = String.format("following-sibling::*//%s[%s[%s[text()=\"%s\"]]]",
        XelementName.F_ARRAY_REF,
        XelementName.VAR_REF,
        XelementName.VAR,
        identifier
    );

    List<XarrayRef> refs = new ArrayList<>();
    try {
      XPathFactory xPathfactory = XPathFactory.newInstance();
      XPath xpath = xPathfactory.newXPath();
      XPathExpression xpathExpr = xpath.compile(s1);
      NodeList output = (NodeList) xpathExpr.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element arraRef = (Element) output.item(i);
        refs.add(new XarrayRef(arraRef));
      }
    } catch (XPathExpressionException ignored) {
      System.out.println("EX");
    }
    return refs;

  }

  /**
   * Get the first assignment statement for an array reference.
   * @param from      Statement to look from.
   * @param arrayName Identifier of the array.
   * @return The assignement statement if found. Null otherwise.
   */
  public static XassignStatement getFirstArrayAssign(XbaseElement from,
                                                           String arrayName)
  {
    String s1 = String.format(
        "following::%s[%s[%s[%s[text()=\"%s\"]] and position()=1]]",
        XelementName.F_ASSIGN_STMT,
        XelementName.F_ARRAY_REF,
        XelementName.VAR_REF,
        XelementName.VAR,
        arrayName
    );

    try {
      XPathFactory xPathfactory = XPathFactory.newInstance();
      XPath xpath = xPathfactory.newXPath();
      XPathExpression xpathExpr = xpath.compile(s1);
      NodeList output = (NodeList) xpathExpr.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      if(output.getLength() == 0){
        return null;
      }
      Element assign = (Element) output.item(0);
      return new XassignStatement(assign);

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
   * @return List of do statements elements (outter most loops for each nested
   * group) found between the "from" element and the end pragma.
   */
  public static List<XdoStatement> findDoStatement(XbaseElement from,
                                                   Xpragma endPragma,
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
            XelementName.DO_STMT,
            XelementName.VAR,
            inductionVars.get(i));
      } else {
        tempQuery = String.format("%s[%s[text()=\"%s\"] and %s[%s]]",
            XelementName.DO_STMT,
            XelementName.VAR,
            inductionVars.get(i),
            XelementName.BODY,
            dynamic_part_s1); // Including previsouly formed xpath query
      }
      dynamic_part_s1 = tempQuery;
    }
    s1 = s1 + dynamic_part_s1;
    List<XdoStatement> doStatements = new ArrayList<>();
    try {
      XPathFactory xPathfactory = XPathFactory.newInstance();
      XPath xpath = xPathfactory.newXPath();
      XPathExpression xpathExpr = xpath.compile(s1);
      NodeList output = (NodeList) xpathExpr.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element el = (Element) output.item(i);
        XdoStatement doStmt = new XdoStatement(el);
        if(doStmt.getLineNo() != 0 && doStmt.getLineNo() < endPragma.getLineNo()){
          doStatements.add(doStmt);
        }
      }
    } catch (XPathExpressionException ignored) {
      System.err.println(ignored);
    }
    return doStatements;
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
  public static List<XarrayRef> getAllArrayReferencesByOffsets(
      XbaseElement from,
      String identifier,
      List<Integer> offsets)
  {

    String offsetXpath = "";
    for (int i = 0; i < offsets.size(); ++i){
      if(offsets.get(i) == 0){
        offsetXpath +=
            String.format("%s[position()=%s and %s]",
                XelementName.ARRAY_INDEX,
                i+1,
                XelementName.VAR
            );
      } else if(offsets.get(i) > 0) {
        offsetXpath +=
            String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                XelementName.ARRAY_INDEX,
                i+1,
                XelementName.MINUS_EXPR,
                XelementName.VAR,
                XelementName.F_INT_CONST,
                offsets.get(i));
      } else {
        offsetXpath +=
            String.format("%s[position()=%s and %s[%s and %s[text()=\"%s\"]]]",
                XelementName.ARRAY_INDEX,
                i+1,
                XelementName.MINUS_EXPR,
                XelementName.VAR,
                XelementName.F_INT_CONST,
                Math.abs(offsets.get(i)));
      }
      if(i != offsets.size()-1){
        offsetXpath += " and ";
      }
    }

    // Start of the Xpath query
    String xpathQuery = String.format(".//%s[%s[%s[text()=\"%s\"]] and %s]",
        XelementName.F_ARRAY_REF,
        XelementName.VAR_REF,
        XelementName.VAR,
        identifier,
        offsetXpath
    );

    List<XarrayRef> arrayRefs = new ArrayList<>();
    try {
      XPathFactory xPathfactory = XPathFactory.newInstance();
      XPath xpath = xPathfactory.newXPath();
      XPathExpression xpathExpr = xpath.compile(xpathQuery);
      NodeList output = (NodeList) xpathExpr.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element arrayRef = (Element) output.item(i);
        arrayRefs.add(new XarrayRef(arrayRef));
      }
    } catch (XPathExpressionException ignored) { }
    return arrayRefs;
  }

  /**
   * Find all real constants in the direct children of the given parent.
   * @param parent Root element to search from.
   * @return A list of all found real constants.
   */
  public static List<XrealConstant> getRealConstants(XbaseElement parent){
    List<XrealConstant> elements = new ArrayList<>();
    Node n = parent.getBaseElement().getFirstChild();
    while(n != null){
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        if(el.getTagName().equals(XelementName.F_REAL_CONST)) {
          XrealConstant ref = new XrealConstant(el);
          elements.add(ref);
        }
      }
      n = n.getNextSibling();
    }
    return elements;
  }

  /**
   * Insert a function call at the end of a do statement.
   * @param loop The do statement to insert in.
   * @param call The function call to be inserted.
   */
  public static void insertFctCallIntoLoop(XdoStatement loop, XfunctionCall call){
    loop.getBody().getBaseElement().appendChild(call.getBaseElement().getParentNode());
  }

  /**
   * Find function definition in the ancestor.
   * @param child The child element to search from.
   * @return A XfunctionDefinition object if found. Null otherwise.
   */
  public static XfunctionDefinition findParentFctDef(XbaseElement child){
    return findParentOfType(child, XfunctionDefinition.class);
  }

  /**
   * Find do statment in which the child is included if any.
   * @param child The child element to search from.
   * @return A XdoStatement object if found. Null otherwise.
   */
  public static XdoStatement findParentDoStmt(XbaseElement child){
    return findParentOfType(child, XdoStatement.class);
  }

  /**
   * Find module definition element in which the child is included if any.
   * @param child The child element to search from.
   * @return A XmoduleDefinition object if found. Null otherwise.
   */
  public static XmoduleDefinition findParentModuleDefinition(XbaseElement child)
  {
    return findParentOfType(child, XmoduleDefinition.class);
  }

  /**
   * Find if statement element in which the child is included if any.
   * @param child The child element to search from.
   * @return A XifStatement object if found. Null otherwise.
   */
  public static XifStatement findParentIfStatement(XbaseElement child)
  {
    return findParentOfType(child, XifStatement.class);
  }



  /**
   * Find a pragma element in the previous nodes containing a given keyword.
   * @param from    Element to start from.
   * @param keyword Keyword to be found in the pragma.
   * @return The pragma if found. Null otherwise.
   */
  public static Xpragma findPreviousPragma(XbaseElement from, String keyword){
    if(from == null || from.getBaseElement() == null){
      return null;
    }
    Node prev = from.getBaseElement().getPreviousSibling();
    Node parent = from.getBaseElement();
    do {
      while (prev != null) {
        if (prev.getNodeType() == Node.ELEMENT_NODE) {
          Element element = (Element) prev;
          if (element.getTagName().equals(XelementName.PRAGMA_STMT)
              && element.getTextContent().toLowerCase().
              contains(keyword.toLowerCase()))
          {
            return new Xpragma(element);
          }
        }
        prev = prev.getPreviousSibling();
      }
      parent = parent.getParentNode();
      prev = parent;
    } while(parent != null);
    return null;
  }

  /**
   * Find do statement element.
   * @param fctDef  Function definition to search in.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XdoStatement object if found. Null otherwise.
   */
  public static XdoStatement findDoStatement(XfunctionDefinition fctDef, boolean any){
    Xbody body = fctDef.getBody();
    return XelementHelper.findDoStatement(body, any);
  }

  /**
   * Find var element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xvar object if found. Null otherwise.
   */
  public static Xvar findVar(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xvar.class);
  }

  /**
   * Find varRef element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XvarRef object if found. Null otherwise.
   */
  public static XvarRef findVarRef(XbaseElement parent, boolean any){
    return findXelement(parent, any, XvarRef.class);
  }

  /**
   * Find indexRange element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XindexRange object if found. Null otherwise.
   */
  public static XindexRange findIndexRange(XbaseElement parent, boolean any){
    return findXelement(parent, any, XindexRange.class);
  }

  /**
   * Find arrayIndex element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XarrayIndex object if found. Null otherwise.
   */
  public static XarrayIndex findArrayIndex(XbaseElement parent, boolean any){
    return findXelement(parent, any, XarrayIndex.class);
  }

  /**
   * Find name element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xname object if found. Null otherwise.
   */
  public static Xname findName(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xname.class);
  }

  /**
   * Find value element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xvalue object if found. Null otherwise.
   */
  public static Xvalue findValue(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xvalue.class);
  }

  /**
   * Find lValueModel element at given position.
   * @param parent   Root element to search from.
   * @param position Position of the element to be found in the parent children
   *                 list.
   * @return A XLValueModel object if found. Null otherwise.
   */
  public static XLValueModel findLValueModel(XbaseElement parent, int position){
    Element element = getXthChildElement(parent.getBaseElement(), position);
    if(element == null){
      return null;
    }
    switch (element.getTagName()) {
      case XelementName.VAR:
        return new XLValueModel(new Xvar(element));
      case XelementName.F_ARRAY_REF:
        return new XLValueModel(new XarrayRef(element));
      case XelementName.F_CHAR_REF:
      case XelementName.F_MEMBER_REF:
      case XelementName.F_COARRAY_REF:
        return null; // TODO when classes are available
      default:
        return null;
    }
  }

  /**
   * Find exprModel element at given position.
   * @param parent   Root element to search from.
   * @param position Position of the element to be found in the parent children
   *                 list.
   * @return A XexprModel object if found. Null otherwise.
   */
  public static XexprModel findExprModel(XbaseElement parent, int position){
    /** An exprModel can be of the following type
     *   - FintConstant, FrealConstant, FcomplexConstant, FcharacterConstant,
     *     FlogicalConstant
     *   TODO FarrayConstructor, FstructConstructor
     *   - FarrayConstructor, FstructConstructor
     *   - Var
     *   TODO FcharacterRef, FmemberRef, FcoArrayRef
     *   - FarrayRef, FcharacterRef, FmemberRef, FcoArrayRef, varRef
     *   - functionCall
     *   - plusExpr, minusExpr, mulExpr, divExpr, FpowerExpr, FconcatExpr
     *     logEQExpr, logNEQExpr, logGEExpr, logGTExpr, logLEExpr, logLTExpr,
     *     logAndExpr, logOrExpr, logEQVExpr, logNEQVExpr, logNotExpr,
     *     unaryMinusExpr, userBinaryExpr, userUnaryExpr
     *   TODO FdoLoop
     *   - FdoLoop
     */

    Element element = getXthChildElement(parent.getBaseElement(), position);
    if(element == null){
      return null;
    }

    switch (element.getTagName()){
      case XelementName.F_INT_CONST:
        return new XexprModel(new XintConstant(element));
      case XelementName.F_REAL_CONST:
        return new XexprModel(new XrealConstant(element));
      case XelementName.F_LOGICAL_CONST:
        return new XexprModel(new XlogicalConstant(element));
      case XelementName.F_COMPLEX_CONST:
        return new XexprModel(new XcomplexConstant(element));
      case XelementName.F_CHAR_CONST:
        return new XexprModel(new XcharacterConstant(element));
      case XelementName.VAR:
        return new XexprModel(new Xvar(element));
      case XelementName.FCT_CALL:
        return new XexprModel(new XfunctionCall(element));
      case XelementName.F_ARRAY_REF:
        return new XexprModel(new XarrayRef(element));
      case XelementName.VAR_REF:
        return new XexprModel(new XvarRef(element));
      // binary expression
      case XelementName.DIV_EXPR:
      case XelementName.F_CONCAT_EXPR:
      case XelementName.F_POWER_EXPR:
      case XelementName.LOG_AND_EXPR:
      case XelementName.LOG_EQ_EXPR:
      case XelementName.LOG_EQV_EXPR:
      case XelementName.LOG_GE_EXPR:
      case XelementName.LOG_GT_EXPR:
      case XelementName.LOG_LE_EXPR:
      case XelementName.LOG_LT_EXPR:
      case XelementName.LOG_NEQ_EXPR:
      case XelementName.LOG_NEWV_EXPR:
      case XelementName.LOG_OR_EXPR:
      case XelementName.MINUS_EXPR:
      case XelementName.MUL_EXPR:
      case XelementName.PLUS_EXPR:
      case XelementName.USER_BINARY_EXPR:
        return new XexprModel(new XbinaryExpr(element));

      // unary expression
      case XelementName.LOG_NOT_EXPR:
      case XelementName.UNARY_MINUS_EXPR:
      case XelementName.USER_UNARY_EXPR:
        return new XexprModel(new XunaryExpr(element));


      default:
        return null;
    }
  }

  /**
   * The inner element of a varRef is one of the following:
   * - Var
   * - FmemberRef
   * - FarrayRef
   * - FcharacterRef
   * - FcoArrayRef
   * @param parent The root element to search form.
   * @return The varRef inner element as a XbaseElement derived type.
   */
  public static XbaseElement findVarRefInnerElement(XbaseElement parent){
    Element element = getFirstChildElement(parent.getBaseElement());
    if(element == null){
      return null;
    }

    switch (element.getTagName()) {
      case XelementName.VAR:
        return new Xvar(element);
      case XelementName.F_MEMBER_REF:
        return null; // TODO move to XmemberRef
      case XelementName.F_ARRAY_REF:
        return new XarrayRef(element);
      case XelementName.F_CHAR_REF:
        return null; // TODO move to XcharacterRef
      case XelementName.F_COARRAY_REF:
        return null; // TODO move to XcoArrayRef
      default:
        return null;
    }
  }

  /**
   * Find constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xconstant object if found. Null otherwise.
   */
  public static Xconstant findConstant(XbaseElement parent, boolean any){
    // FintConstant, FrealConstant, FcomplexConstant, FcharacterConstant,
    // FlogicalConstant

    XintConstant intConst = findIntConstant(parent, any);
    if(intConst != null){
      return intConst;
    }

    XrealConstant realConst = findRealConstant(parent, any);
    if(realConst != null){
      return realConst;
    }

    XcomplexConstant complexConst = findComplexConstant(parent, any);
    if(complexConst != null){
      return complexConst;
    }

    XcharacterConstant charConst = findCharacterConstant(parent, any);
    if(charConst != null){
      return charConst;
    }

    XlogicalConstant logConst = findLogicalConstant(parent, any);
    if(logConst != null){
      return logConst;
    }

    return null;
  }

  /**
   * Find integer constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XintConstant object if found. Null otherwise.
   */
  public static XintConstant findIntConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XintConstant.class);
  }

  /**
   * Find real constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XrealConstant object if found. Null otherwise.
   */
  public static XrealConstant findRealConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XrealConstant.class);
  }

  /**
   * Find complex constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XcomplexConstant object if found. Null otherwise.
   */
  public static XcomplexConstant findComplexConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XcomplexConstant.class);
  }

  /**
   * Find character constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XcharacterConstant object if found. Null otherwise.
   */
  public static XcharacterConstant findCharacterConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XcharacterConstant.class);
  }

  /**
   * Find logical constant element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XlogicalConstant object if found. Null otherwise.
   */
  public static XlogicalConstant findLogicalConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XlogicalConstant.class);
  }

  /**
   * Find condition element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xcondition object if found. Null otherwise.
   */
  public static Xcondition findCondition(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xcondition.class);
  }

  /**
   * Find then element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xthen object if found. Null otherwise.
   */
  public static Xthen findThen(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xthen.class);
  }

  /**
   * Find else element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xelse object if found. Null otherwise.
   */
  public static Xelse findElse(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xelse.class);
  }

  /**
   * Find arguments element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XargumentsTable object if found. Null otherwise.
   */
  public static XargumentsTable findArgumentsTable(XbaseElement parent, boolean any){
    return findXelement(parent, any, XargumentsTable.class);
  }

  /**
   * Find lowerBound element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XlowerBound object if found. Null otherwise.
   */
  public static XlowerBound findLowerBound(XbaseElement parent, boolean any){
    return findXelement(parent, any, XlowerBound.class);
  }

  /**
   * Find upperBound element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XupperBound object if found. Null otherwise.
   */
  public static XupperBound findUpperBound(XbaseElement parent, boolean any){
    return findXelement(parent, any, XupperBound.class);
  }

  /**
   * Find step element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xstep object if found. Null otherwise.
   */
  public static Xstep findStep(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xstep.class);
  }

  /**
   * Find body element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xbody object if found. Null otherwise.
   */
  public static Xbody findBody(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xbody.class);
  }

  /**
   * Find do statement element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XdoStatement object if found. Null otherwise.
   */
  public static XdoStatement findDoStatement(XbaseElement parent, boolean any){
    return findXelement(parent, any, XdoStatement.class);
  }

  /**
   * Find the direct next do statement element.
   * @param from The element to search from. Direct next sibling is searched.
   * @return A XdoStatement object if it directly follows the given from
   * element. Null otherwise.
   */
  public static XdoStatement findNextDoStatement(XbaseElement from){
    return findNextElementOfType(from, XdoStatement.class);
  }

  /**
   * Find the direct next do statement element.
   * @param from  The element to search from. Direct next sibling is searched.
   * @param until The element to search until.
   * @return A XdoStatement object if it directly follows the given from
   * element. Null otherwise.
   */
  public static XdoStatement findNextDoStatement(XbaseElement from,
                                                 XbaseElement until)
  {
    return findNextElementOfType(from, until, XdoStatement.class);
  }

  /**
   * Find symbols element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XsymbolsTable object if found. Null otherwise.
   */
  public static XsymbolTable findSymbols(XbaseElement parent, boolean any){
    return findXelement(parent, any, XsymbolTable.class);
  }

  /**
   * Find declarations element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XdeclTable object if found. Null otherwise.
   */
  public static XdeclTable findDeclarations(XbaseElement parent, boolean any){
    return findXelement(parent, any, XdeclTable.class);
  }

  /**
   * Find type table elements.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XtypeTable object if found. Null otherwise.
   */
  public static XtypeTable findTypeTable(XcodeProgram parent, boolean any){
    return findXelement(parent, any, XtypeTable.class);
  }

  /**
   * Find global symbols element in the XcodeML representation.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XglobalSymbolTable object if found. Null otherwise.
   */
  public static XglobalSymbolTable findGlobalSymbols(XcodeProgram parent,
    boolean any)
  {
    return findXelement(parent, any, XglobalSymbolTable.class);
  }

  /**
   * Find global declarations element in the XcodeML representation.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XglobalSymbolTable object if found. Null otherwise.
   */
  public static XglobalDeclTable findGlobalDeclarations(XcodeProgram parent,
                                                        boolean any)
  {
    return findXelement(parent, any, XglobalDeclTable.class);
  }

  /**
   * Find params in the XcodeML representation.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xparams object if found. Null otherwise.
   */
  public static Xparams findParams(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xparams.class);
  }

  /**
   * Find number of index ranges in an element.
   * @param parent  Root element to search from.
   * @return The number of index ranges found.
   */
  public static int findNumberOfRange(XbaseElement parent){
    int indexCounter = 0;
    Node node = parent.getBaseElement().getFirstChild();
    while(node != null){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        if(element.getTagName().equals(XelementName.INDEX_RANGE)){
          ++indexCounter;
        }
      }
    }
    return indexCounter;
  }

  /**
   * Find all the index elements (arrayIndex and indexRange) in an element.
   * @param parent  Root element to search from.
   * @return A list of all index ranges found.
   */
  public static List<Xindex> findIndexes(XbaseElement parent){
    List<Xindex> indexRanges = new ArrayList<>();
    if(parent == null || parent.getBaseElement() == null){
      return indexRanges;
    }

    Node node = parent.getBaseElement().getFirstChild();
    while (node != null){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        switch (element.getTagName()){
          case XelementName.ARRAY_INDEX:
            indexRanges.add(new XarrayIndex(element));
            break;
          case XelementName.INDEX_RANGE:
            indexRanges.add(new XindexRange(element));
            break;
        }
      }
      node = node.getNextSibling();
    }

    return indexRanges;
  }

  /**
   * Find all the name elements in an element.
   * @param parent  Root element to search from.
   * @return A list of all name elements found.
   */
  public static List<Xname> findAllNames(XbaseElement parent){
    List<Xname> names = new ArrayList<>();
    if(parent == null || parent.getBaseElement() == null){
      return names;
    }

    Node node = parent.getBaseElement().getFirstChild();
    while (node != null){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        if(element.getTagName().equals(XelementName.NAME)) {
          names.add(new Xname(element));
        }
      }
      node = node.getNextSibling();
    }

    return names;
  }

  /**
   * Find len element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xlength object if found. Null otherwise.
   */
  public static Xlength findLen(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xlength.class);
  }

  /**
   * Find kind element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A Xkind object if found. Null otherwise.
   */
  public static Xkind findKind(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xkind.class);
  }

  /**
   * Find function call element.
   * @param parent  Root element to search from.
   * @param any     If true, find in any nested element under parent. If
   *                false, only direct children are search for.
   * @return        A XfunctionCall object if found. Null otherwise.
   */
  public static XfunctionCall findFctCall(XbaseElement parent, boolean any){
    return findXelement(parent, any, XfunctionCall.class);
  }

  /**
   * Find a function call element nested in the given expression statement.
   * @param exprStmt The expression statement to search from.
   * @return A function call element if found. Null otherwise.
   */
  public static XfunctionCall findFctCall(XexprStatement exprStmt){
    if(exprStmt == null){
      return null;
    }

    NodeList nodeList = exprStmt.getBaseElement().getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(XelementName.FCT_CALL)){
          return new XfunctionCall(element);
        }
      }
    }
    return null;
  }

  /**
   * Find if there is a expr statement directly after the given element.
   * @param from  The element to search from
   * @return An expr statement element if found. Null otherwise.
   */
  public static XexprStatement findNextExprStatement(XbaseElement from){
    return findNextElementOfType(from, XexprStatement.class);
  }

  /**
   * Find if there is a do statement directly after the given element.
   * @param from  The element to search from
   * @return A do statement element if found. Null otherwise.
   */
  public static XdoStatement findDirectNextDoStmt(XbaseElement from){
    return findDirectNextElement(from, XdoStatement.class);
  }

  /**
   * Find if there is an if statement directly after the given element.
   * @param from  The element to search from
   * @return An if statement element if found. Null otherwise.
   */
  public static XifStatement findDirectNextIfStmt(XbaseElement from){
    return findDirectNextElement(from, XifStatement.class);
  }

  /**
   * Find if there is an assign statement directly after the given element.
   * @param from  The element to search from
   * @return An assign statement element if found. Null otherwise.
   */
  public static XassignStatement findDirectNextAssignStmt(XbaseElement from){
    return findDirectNextElement(from, XassignStatement.class);
  }


  /**
   * Delete all the elements between the two given pragmas.
   * @param start The start pragma. Deletion start from next element.
   * @param end   The end pragma. Deletion end just before this element.
   */
  public static void deleteBetween(Xpragma start, Xpragma end){
    ArrayList<Element> toDelete = new ArrayList<>();
    Node node = start.getBaseElement().getNextSibling();
    while (node != null && node != end.getBaseElement()){
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
   * Find all the pragma element in an XcodeML tree.
   * @param xcodeml The XcodeML program to search in.
   * @return A list of all pragmas found in the XcodeML program.
   */
  public static List<Xpragma> findAllPragmas(XcodeProgram xcodeml){
    NodeList pragmaList = xcodeml.getDocument()
      .getElementsByTagName(XelementName.PRAGMA_STMT);
    List<Xpragma> pragmas = new ArrayList<>();
    for (int i = 0; i < pragmaList.getLength(); i++) {
      Node pragmaNode = pragmaList.item(i);
      if (pragmaNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) pragmaNode;
        pragmas.add(new Xpragma(element));
      }
    }
    return pragmas;
  }

  /**
   * Check if the two element are direct children of the same parent element.
   * @param e1 First element.
   * @param e2 Second element.
   * @return True if the two element are direct children of the same parent.
   * False otherwise.
   */
  public static boolean hasSameParentBlock(XbaseElement e1, XbaseElement e2){
    if(e1 == null || e2 == null || e1.getBaseElement() == null
      || e2.getBaseElement() == null)
    {
      return false;
    }

    return e1.getBaseElement().getParentNode()
        == e2.getBaseElement().getParentNode();
  }

  /**
   * Insert all the statements from a given body at the end of another body
   * @param originalBody The body in which the extra body will be appended
   * @param extraBody    The body that will be appended to the original body
   * @throws IllegalTransformationException if one of the body or their base
   *         element is null.
   */
  public static void appendBody(Xbody originalBody, Xbody extraBody)
    throws IllegalTransformationException
  {
    if(originalBody == null || originalBody.getBaseElement() == null
      || extraBody == null || extraBody.getBaseElement() == null)
    {
      throw new IllegalTransformationException("One of the body is null.");
    }

    // Append content of loop-body (loop) to this loop-body
    Node childNode = extraBody.getBaseElement().getFirstChild();
    while(childNode != null){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        originalBody.getBaseElement().appendChild(childNode);
      }
      childNode = nextChild;
    }
  }

  /**
   * Extract the body of a do statement and place it directly after it.
   * @param loop The do statement containing the body to be extracted.
   */
  public static void extractBody(XdoStatement loop){
    Element loopElement = loop.getBaseElement();
    Element body = XelementHelper.findFirstElement(loopElement,
      XelementName.BODY);

    Node refNode = loopElement;
    if(body == null){
      return;
    }
    for(Node childNode = body.getFirstChild(); childNode!=null;){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        XelementHelper.insertAfter(refNode, childNode);
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
   * @return true if the output could be write without problems.
   */
  public static boolean writeXcodeML(XcodeProgram xcodeml, String outputFile, int indent) {
    try {
      XelementHelper.cleanEmptyTextNodes(xcodeml.getDocument());
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
    } catch (TransformerConfigurationException ex){
      xcodeml.addError("Cannot output file: " + ex.getMessage(), 0);
      return false;
    } catch (TransformerException ex){
      xcodeml.addError("Cannot output file: " + ex.getMessage(), 0);
      return false;
    }
    return true;
  }

  /**
   * Removes text nodes that only contains whitespace. The conditions for
   * removing text nodes, besides only containing whitespace, are: If the
   * parent node has at least one child of any of the following types, all
   * whitespace-only text-node children will be removed: - ELEMENT child -
   * CDATA child - COMMENT child.
   * @param parentNode Root node to start the cleaning.
   */
  public static void cleanEmptyTextNodes(Node parentNode) {
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

  /**
   * Validate a string attribute.
   * @param doc         Document in which the attribute must be validated.
   * @param attrValue   Attribute value expected.
   * @param xpathQuery  Xpath query to locate the attribute value.
   * @return True if the attribute validates. False otherwise.
   * @throws Exception if xpathQuery cannot be executed.
   */
  public static boolean validateStringAttribute(Document doc, String attrValue
    , String xpathQuery) throws Exception
  {
    XPathFactory xPathfactory = XPathFactory.newInstance();
    XPath xpath = xPathfactory.newXPath();
    XPathExpression getVersion = xpath.compile(xpathQuery);
    String outputValue = (String) getVersion.evaluate(doc,
      XPathConstants.STRING);
    return outputValue.equals(attrValue);
  }

  /**
   * Insert an element just before a reference element.
   * @param ref    The reference element.
   * @param insert The element to be inserted.
   */
  public static void insertBefore(XbaseElement ref, XbaseElement insert){
    ref.getBaseElement().getParentNode().insertBefore(insert.getBaseElement(),
        ref.getBaseElement());
  }

  /**
   * Insert an element just after a reference element.
   * @param refElement  The reference element.
   * @param element     The element to be inserted.
   */
  public static void insertAfter(XbaseElement refElement, XbaseElement element){
    XelementHelper.insertAfter(refElement.getBaseElement(),
        element.getBaseElement());
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

    return nodeType == Node.ELEMENT_NODE
        || nodeType == Node.CDATA_SECTION_NODE
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
   * Find an element of Class T in the nested elements under parent.
   * @param parent        XbaseElement to search from.
   * @param any           If true, find in any nested element under parent. If
   *                      false, only direct children are search for.
   * @param xElementClass Element's class to be found.
   * @param <T>           Derived class of XbaseElement.
   * @return An instance of T class if an element is found. Null if no element
   * is found.
   */
  private static <T extends XbaseElement> T findXelement(XbaseElement parent,
    boolean any, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || parent == null
      || parent.getBaseElement() == null)
    {
      return null;
    }
    Element element = findElement(parent, elementName, any);
    if (element != null){
      try{
        return xElementClass.
          getDeclaredConstructor(Element.class).newInstance(element);
      } catch(Exception ex){
        return null;
      }
    }
    return null;
  }

  /**
   * Find any element of the the given Class in the direct children of from
   * element. Only first level children are search for.
   * @param from          XbaseElement to search from.
   * @param xElementClass Element's class to be found.
   * @param <T>           Derived class of XbaseElement
   * @return The first element found under from element. Null if no element is
   * found.
   */
  private static <T extends XbaseElement> T findNextElementOfType(
    XbaseElement from, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || from == null || from.getBaseElement() == null){
      return null;
    }
    Node nextNode = from.getBaseElement().getNextSibling();
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(elementName)){
          try {
            return xElementClass.
              getDeclaredConstructor(Element.class).newInstance(element);
          } catch(Exception ex){
            return null;
          }
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  /**
   * Find any element of the the given Class in the direct children of from
   * element until the end element is reached.
   * Only first level children are search for.
   * @param from          XbaseElement to search from.
   * @param until         XbaseElement to search until.
   * @param xElementClass Element's class to be found.
   * @param <T>           Derived class of XbaseElement
   * @return The first element found under from element. Null if no element is
   * found.
   */
  private static <T extends XbaseElement> T findNextElementOfType(
      XbaseElement from, XbaseElement until, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || from == null || until == null
        || from.getBaseElement() == null)
    {
      return null;
    }
    Node nextNode = from.getBaseElement().getNextSibling();
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element == until.getBaseElement()){ // End element is reached
          return null;
        }
        if(element.getTagName().equals(elementName)){
          try {
            return xElementClass.
                getDeclaredConstructor(Element.class).newInstance(element);
          } catch(Exception ex){
            return null;
          }
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  /**
   * Find element of the the given Class that is directly after the given from
   * element.
   * @param from          XbaseElement to search from.
   * @param xElementClass Element's class to be found.
   * @param <T>           Derived class of XbaseElement.
   * @return Instance of the xElementClass. Null if no element is found.
   */
  private static <T extends XbaseElement> T findDirectNextElement(
    XbaseElement from, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || from == null || from.getBaseElement() == null){
      return null;
    }
    Node nextNode = from.getBaseElement().getNextSibling();
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(elementName)){
          try{
            return xElementClass.
              getDeclaredConstructor(Element.class).newInstance(element);
          } catch(Exception ex){
            return null;
          }
        }
        return null;
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  /**
   * Find a parent element from a child in the ancestors.
   * @param from The child element to search from.
   * @return A XbaseElement object if found. Null otherwise.
   */
  private static <T extends XbaseElement> T findParentOfType(
      XbaseElement from, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || from == null || from.getBaseElement() == null){
      return null;
    }
    Node parent = from.getBaseElement().getParentNode();
    while(from.getBaseElement().getParentNode() != null){
      if (parent.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) parent;
        if(element.getTagName().equals(elementName)){
          try{
            return xElementClass.
                getDeclaredConstructor(Element.class).newInstance(element);
          } catch(Exception ex){
            return null;
          }
        }
      }
      parent = parent.getParentNode();
    }
    return null;
  }

  /**
   * Find the first element with tag corresponding to elementName.
   * @param parent      The root element to search from.
   * @param elementName The tag of the element to search for.
   * @param any         If true, find in any nested element under parent. If
   *                    false, only direct children are search for.
   * @return first element found. Null if no element is found.
   */
  private static Element findElement(XbaseElement parent, String elementName, boolean any){
    return findElement(parent.getBaseElement(), elementName, any);
  }

  /**
   * Find the first element with tag corresponding to elementName.
   * @param parent      The root element to search from.
   * @param elementName The tag of the element to search for.
   * @param any         If true, find in any nested element under parent. If
   *                    false, only direct children are search for.
   * @return first element found. Null if no element is found.
   */
  private static Element findElement(Element parent, String elementName, boolean any){
    if(any){
      return findFirstElement(parent, elementName);
    } else {
      return findFirstChildElement(parent, elementName);
    }
  }

  private static Element findFirstElement(XbaseElement parent, String elementName){
    return findFirstElement(parent.getBaseElement(), elementName);
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
   * Get the first child element.
   * @param parent Root element to search form.
   * @return First found element.
   */
  private static Element getFirstChildElement(Element parent){
    NodeList nodeList = parent.getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if (nextNode.getNodeType() == Node.ELEMENT_NODE) {
        return (Element) nextNode;
      }
    }
    return null;
  }

  /**
   * Get the xth child element.
   * @param parent   Root element to search form.
   * @param position Position of the element to be found. Start at 0.
   * @return Element found at position.
   */
  private static Element getXthChildElement(Element parent, int position){
    int crtIndex = 0;
    NodeList nodeList = parent.getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if (nextNode.getNodeType() == Node.ELEMENT_NODE && crtIndex == position) {
        return (Element) nextNode;
      } else if (nextNode.getNodeType() == Node.ELEMENT_NODE){
        ++crtIndex;
      }
    }
    return null;
  }

  /**
   * Create an empty XbinaryExpr object with the given tag.
   * @param exprTag The tag associated with the specialized binary expression.
   * @param xcodeml The current XcodeML program.
   * @return A new Xbinary object.
   * @throws IllegalTransformationException If the tag is not associated with
   * any binary expression.
   */
  public static XbinaryExpr createEmpty(String exprTag, XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(XelementName.isBinaryExprTag(exprTag)){
      Element element = xcodeml.getDocument().createElement(exprTag);
      return new XbinaryExpr(element);
    }
    throw new IllegalTransformationException("No binary expression with tag:" +
        exprTag);
  }

  /**
   * Create an empty of the given class element in the given program.
   * @param xElementClass The class to be created
   * @param xcodeml       The current XcodeML program.
   * @param <T>           Type of the class to be created.
   * @return An empty arrayIndex element.
   * @throws IllegalTransformationException if element cannot be created.
   */
  public static <T extends XbaseElement> T createEmpty(Class<T> xElementClass,
                                                       XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName != null){
      Element element = xcodeml.getDocument().createElement(elementName);
      try {
        return xElementClass.
            getDeclaredConstructor(Element.class).newInstance(element);
      } catch(Exception ex){
        throw new IllegalTransformationException("Cannot create new statement: "
            + elementName);
      }
    }
    throw new IllegalTransformationException("Undefined statement for classe:" +
      xElementClass.toString());
  }

  /**
   * Get the depth of an element in the AST.
   * @param element The element for which the depth is computed.
   * @return A depth value greater or equal to 0.
   */
  public static int getDepth(XbaseElement element) {
    if(element == null || element.getBaseElement() == null){
      return -1;
    }
    return getDepth(element.getBaseElement());
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
  public static void shiftStatementsInBody(XbaseElement from,
                                           XbaseElement until, Xbody targetBody)
  {
    Node currentSibling = from.getBaseElement().getNextSibling();
    Node firstStatementInBody = targetBody.getBaseElement().getFirstChild();
    while(currentSibling != null && currentSibling != until.getBaseElement()){
      Node nextSibling = currentSibling.getNextSibling();
      targetBody.getBaseElement().insertBefore(currentSibling,
          firstStatementInBody);
      currentSibling = nextSibling;
    }
  }


  /**
   * Copy the enhanced information from an element to a target element.
   * Enhanced information include line number and original file name.
   * @param base    Base element to copy information from.
   * @param target  Target element to copy information to.
   */
  public static void copyEnhancedInfo(XenhancedElement base,
                                      XenhancedElement target)
  {
    target.setLine(base.getLineNo());
    target.setFile(base.getFile());
  }

}
