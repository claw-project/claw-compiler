/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.helper;

import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.xelement.*;

import cx2x.xcodeml.xnode.Xattr;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;
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
                                                           Xnode fctCall)
  {
    if(xcodeml.getBaseElement() == null){
      return null;
    }
    String name = fctCall.findNode(Xcode.NAME).getValue();
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
    if(module.getElement() == null){
      return null;
    }
    NodeList nList = module.getElement().
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
        getElementsByTagName(XelementName.F_ARRAY_REF);
    for (int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Xnode ref = new Xnode((Element) n);
        if(ref.find(Xcode.VARREF, Xcode.VAR).getValue().toLowerCase().
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
                                           List<Xnode> list2){
    if(list1.size() != list2.size()){
      return false;
    }

    for(int i = 0; i < list1.size(); ++i){
      if(!isIndexRangeIdentical(list1.get(i), list2.get(i))){
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
  public static List<Xnode> getArrayAssignInBlock(Xnode from, String endPragma){
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
        XelementName.F_ARRAY_REF,
        XelementName.VAR_REF,
        XelementName.VAR,
        identifier
    );
    return getFromXpath(from, s1);
  }

  /**
   * Get the first assignment statement for an array reference.
   * @param from      Statement to look from.
   * @param arrayName Identifier of the array.
   * @return The assignement statement if found. Null otherwise.
   */
  public static Xnode getFirstArrayAssign(Xnode from, String arrayName){
    String s1 = String.format(
        "following::%s[%s[%s[%s[text()=\"%s\"]] and position()=1]]",
        XelementName.F_ASSIGN_STMT,
        XelementName.F_ARRAY_REF,
        XelementName.VAR_REF,
        XelementName.VAR,
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
   * @return List of do statements elements (outter most loops for each nested
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

    return getFromXpath(from, xpathQuery);
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
   * Find a pragma element in the previous nodes containing a given keyword.
   * @param from    Element to start from.
   * @param keyword Keyword to be found in the pragma.
   * @return The pragma if found. Null otherwise.
   */
  public static Xnode findPreviousPragma(Xnode from, String keyword){
    if(from == null || from.getElement() == null){
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
              contains(keyword.toLowerCase()))
          {
            return new Xnode(element);
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
   * Find all the index elements (arrayIndex and indexRange) in an element.
   * @param parent  Root element to search from.
   * @return A list of all index ranges found.
   */
  public static List<Xnode> findIndexes(XbaseElement parent){
    List<Xnode> indexRanges = new ArrayList<>();
    if(parent == null || parent.getBaseElement() == null){
      return indexRanges;
    }

    Node node = parent.getBaseElement().getFirstChild();
    while (node != null){
      if(node.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element)node;
        switch (element.getTagName()){
          case XelementName.ARRAY_INDEX:
          case XelementName.INDEX_RANGE:
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
  public static List<Xname> findAllNames(XbaseElement parent){
    return findAll(parent, Xname.class);
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
  public static void extractBody(Xnode loop){
    Element loopElement = loop.getElement();
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

  /**
   * Validate a string attribute.
   * @param doc       Document in which the attribute must be validated.
   * @param attrValue Attribute value expected.
   * @param query     Xpath query to locate the attribute value.
   * @return True if the attribute validates. False otherwise.
   * @throws Exception if xpathQuery cannot be executed.
   */
  public static boolean validateStringAttribute(Document doc, String attrValue,
                                                String query) throws Exception
  {
    XPathExpression ex = XPathFactory.newInstance().newXPath().compile(query);
    String outputValue = (String) ex.evaluate(doc, XPathConstants.STRING);
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
          return construct(xElementClass, element);
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
    return findOfType(from, xElementClass, true);
  }

  /**
   * Find any element of the the given Class in the direct children of from
   * element. Only first level children are search for.
   * @param from          XbaseElement to search from.
   * @param xElementClass Element's class to be found.
   * @param <T>           Derived class of XbaseElement.
   * @return The first element found under from element. Null if no element is
   * found.
   */
  private static <T extends XbaseElement> T findNextElementOfType(
      XbaseElement from, Class<T> xElementClass)
  {
    return findOfType(from, xElementClass, false);
  }

  /**
   * Find any element of the given Class in the direct children or the ancestors
   * of the from element. In the case if children, only first level children are
   * search for.
   * @param from          XbaseElement to search from.
   * @param xElementClass Element's class to be found.
   * @param ancestor      if true, search in the ancestor. If false, search in
   *                      the children.
   * @param <T>           Derived class of XbaseElement.
   * @return The first element found under or in the ancestors of the from
   * element. Null if no element is found.
   */
  private static <T extends XbaseElement> T findOfType(XbaseElement from,
                                                       Class<T> xElementClass,
                                                       boolean ancestor)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || from == null || from.getBaseElement() == null){
      return null;
    }

    Node nextNode = ancestor ? from.getBaseElement().getParentNode() :
        from.getBaseElement().getNextSibling();

    while(nextNode != null){
      if (nextNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) nextNode;
        if(element.getTagName().equals(elementName)){
          return construct(xElementClass, element);
        }
      }
      nextNode = ancestor ? nextNode.getParentNode() : nextNode.getNextSibling();
    }
    return null;
  }

  /**
   * Constructs an object with the base constrcutor for XbaseElement.
   * @param xElementClass Type of the object.
   * @param element       Element to pass to the constructor.
   * @param <T>           Derived class of XbaseElement.
   * @return A new XbaseElement dervied object or null if the construction
   * fails.
   */
  private static <T extends XbaseElement> T construct(Class<T> xElementClass,
                                                      Element element){
    try{
      return xElementClass.
          getDeclaredConstructor(Element.class).newInstance(element);
    } catch(Exception ex){
      return null;
    }
  }

  /**
   * Get a list of T elements from an xpath query executed from the
   * given element.
   * @param from          Element to start from.
   * @param query         XPath query to be executed.
   * @param xElementClass Type of element to retrieve.
   * @return List of all array references found. List is empty if nothing is
   * found.
   */
  private static <T extends XbaseElement> List<T> getFromXpath(
      XbaseElement from, String query, Class<T> xElementClass)
  {
    List<T> elements = new ArrayList<>();
    try {
      XPathExpression ex = XPathFactory.newInstance().newXPath().compile(query);
      NodeList output = (NodeList) ex.evaluate(from.getBaseElement(),
          XPathConstants.NODESET);
      for (int i = 0; i < output.getLength(); i++) {
        Element element = (Element) output.item(i);
        try{
          T el = xElementClass.
              getDeclaredConstructor(Element.class).newInstance(element);
          elements.add(el);
        } catch(Exception ignored){ }
      }
    } catch (XPathExpressionException ignored) {
    }
    return elements;
  }

  /**
   * Get a list of all inner values from a list of base elements.
   * @param elements List of base elements.
   * @return A list of inner values.
   */
  public static <T extends XbaseElement> List<String> getAllValues(
      List<T> elements)
  {
    List<String> values = new ArrayList<>();
    for(XbaseElement b : elements){
      values.add(b.getValue());
    }
    return values;
  }

  /**
   * Find all elements of the given type in the subtree from the given parent
   * element.
   * @param parent        The element to search from.
   * @param xElementClass Type of element to be searched.
   * @param <T>           Type of the XbaseElement to be searched.
   * @return A list of found elements.
   */
  private static <T extends XbaseElement> List<T> findAll(XbaseElement parent,
                                                          Class<T> xElementClass)
  {
    List<T> elements = new ArrayList<>();
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    if(elementName == null || parent == null) {
      return elements;
    }
    NodeList nodes = parent.getBaseElement().getElementsByTagName(elementName);
    for (int i = 0; i < nodes.getLength(); i++) {
      Node n = nodes.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        try {
          elements.add(xElementClass.
              getDeclaredConstructor(Element.class).newInstance(el));
        } catch(Exception ex){
          return null;
        }

      }
    }
    return elements;
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
        throw new IllegalTransformationException("Cannot create new element: "
            + elementName);
      }
    }
    throw new IllegalTransformationException("Undefined statement for classe:" +
        xElementClass.toString());
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

  /**
   * Copy the whole body element into the destination one. Destination is
   * overwritten.
   * @param from The body to be copied.
   * @param to   The desination of the copied body.
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
      case XelementName.TYPE_F_CHAR:
      case XelementName.TYPE_F_COMPLEX:
      case XelementName.TYPE_F_INT:
      case XelementName.TYPE_F_LOGICAL:
      case XelementName.TYPE_F_REAL:
      case XelementName.TYPE_F_VOID:
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
   * TODO javadoc
   * @param from
   * @return
   */
  public static XfunctionDefinition findParentFunction(Xnode from){
    Xnode fctDef = findParent(Xcode.FFUNCTIONDEFINITION, from);
    if(fctDef == null){
      return null;
    }
    return new XfunctionDefinition(fctDef.getElement());
  }

  /**
   * Find an element in the ancestor of the given element.
   * @param opcode Code of the element to be found.
   * @param from   Element to start the search from.
   * @return The element found. Null if no element is found.
   */
  public static Xnode findParent(Xcode opcode, Xnode from){
    if(from == null){
      return null;
    }

    Node nextNode = from.getElement().getParentNode();

    while(nextNode != null){
      if (nextNode.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) nextNode;
        if(element.getTagName().equals(opcode.code())){
          return new Xnode(element);
        }
      }
      nextNode = nextNode.getParentNode();
    }
    return null;
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
    XelementHelper.insertAfter(refElement.getElement(), element.getElement());
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
      }
      nextNode = nextNode.getNextSibling();
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
   * Compare the iteration range of two do statements.
   * @param e1 First do statement.
   * @param e2 Second do statement.
   * @return True if the iteration range are identical.
   */
  public static boolean hasSameIndexRange(Xnode e1, Xnode e2){
    // The two nodes must be do statement
    if(e1.Opcode() != Xcode.FDOSTATEMENT || e2.Opcode() != Xcode.FDOSTATEMENT){
      return false;
    }

    // TODO XNODE refactoring (have method to get bounds)

    Xnode inductionVar1 = XelementHelper.find(Xcode.VAR, e1, false);
    Xnode inductionVar2 = XelementHelper.find(Xcode.VAR, e2, false);
    Xnode indexRange1 = XelementHelper.find(Xcode.INDEXRANGE, e1, false);
    Xnode indexRange2 = XelementHelper.find(Xcode.INDEXRANGE, e2, false);


    if(!inductionVar1.getValue().toLowerCase().
        equals(inductionVar2.getValue().toLowerCase()))
    {
      return false;
    }
    return isIndexRangeIdentical(indexRange1, indexRange2);
  }

  private static boolean isIndexRangeIdentical(Xnode idx1, Xnode idx2){
    if(idx1.Opcode() != Xcode.INDEXRANGE ||idx2.Opcode() != Xcode.INDEXRANGE){
      return false;
    }

    if(idx1.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE) &&
        idx2.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)){
      return true;
    }

    Xnode low1 = idx1.find(Xcode.LOWERBOUND).getChild(0);
    Xnode up1 = idx1.find(Xcode.UPPERBOUND).getChild(0);

    Xnode low2 = idx2.find(Xcode.LOWERBOUND);
    Xnode up2 = idx2.find(Xcode.UPPERBOUND).getChild(0);

    Xnode s1 = idx1.find(Xcode.STEP);
    Xnode s2 = idx2.find(Xcode.STEP);
    if(s1 != null){
      s1 = s1.getChild(0);
    }
    if(s2 != null){
      s2 = s2.getChild(0);
    }

    if (!low1.getValue().toLowerCase().equals(low2.getValue().toLowerCase())) {
      return false;
    }

    if (!up1.getValue().toLowerCase().equals(up2.getValue().toLowerCase())) {
      return false;
    }

    // step is optional
    return s1 == null && s2 == null ||
        s1.getValue().toLowerCase().equals(s2.getValue().toLowerCase());
  }


  /**
   * Compare the iteration range of two do statements.
   * @param e1 First do statement.
   * @param e2 Second do statement.
   * @return True if the iteration range are identical besides the lower bound.
   */
  public static boolean hasSameIndexRangeBesidesLower(Xnode e1, Xnode e2){
    // The two nodes must be do statement
    if(e1.Opcode() != Xcode.FDOSTATEMENT || e2.Opcode() != Xcode.FDOSTATEMENT){
      return false;
    }

    Xnode inductionVar1 = XelementHelper.find(Xcode.VAR, e1, false);
    Xnode inductionVar2 = XelementHelper.find(Xcode.VAR, e2, false);
    Xnode indexRange1 = XelementHelper.find(Xcode.INDEXRANGE, e1, false);
    Xnode indexRange2 = XelementHelper.find(Xcode.INDEXRANGE, e2, false);
    Xnode up1 = XelementHelper.find(Xcode.UPPERBOUND, indexRange1, false).getChild(0);
    Xnode s1 = XelementHelper.find(Xcode.STEP, indexRange1, false).getChild(0);
    Xnode up2 = XelementHelper.find(Xcode.UPPERBOUND, indexRange2, false).getChild(0);
    Xnode s2 = XelementHelper.find(Xcode.STEP, indexRange2, false).getChild(0);

    if(!inductionVar1.getValue().toLowerCase().
        equals(inductionVar2.getValue().toLowerCase()))
    {
      return false;
    }

    if(indexRange1.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE) &&
        indexRange2.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE)){
      return true;
    }

    if (!up1.getValue().toLowerCase().equals(up2.getValue().toLowerCase())) {
      return false;
    }

    // step is optional
    return s1 == null && s2 == null ||
        s1.getValue().toLowerCase().equals(s2.getValue().toLowerCase());
  }


  public static void swapIterationRange(Xnode e1, Xnode e2){
    // The two nodes must be do statement
    if(e1.Opcode() != Xcode.FDOSTATEMENT || e2.Opcode() != Xcode.FDOSTATEMENT){
      return;
    }

    Xnode inductionVar1 = XelementHelper.find(Xcode.VAR, e1, false);
    Xnode inductionVar2 = XelementHelper.find(Xcode.VAR, e2, false);
    Xnode indexRange1 = XelementHelper.find(Xcode.INDEXRANGE, e1, false);
    Xnode indexRange2 = XelementHelper.find(Xcode.INDEXRANGE, e2, false);
    Xnode low1 = XelementHelper.find(Xcode.LOWERBOUND, indexRange1, false).getChild(0);
    Xnode up1 = XelementHelper.find(Xcode.UPPERBOUND, indexRange1, false).getChild(0);
    Xnode s1 = XelementHelper.find(Xcode.STEP, indexRange1, false).getChild(0);

    Xnode low2 = XelementHelper.find(Xcode.LOWERBOUND, indexRange2, false).getChild(0);
    Xnode up2 = XelementHelper.find(Xcode.UPPERBOUND, indexRange2, false).getChild(0);
    Xnode s2 = XelementHelper.find(Xcode.STEP, indexRange2, false).getChild(0);

    String tmpInduction = inductionVar2.getValue();
    String tmpLower = low2.getValue();
    String tmpUpper = up2.getValue();
    String tmpStep = s2.getValue();

    // Set the range of loop2 to loop1
    inductionVar2.setValue(inductionVar1.getValue());
    low2.setValue(low1.getValue());
    up2.setValue(up1.getValue());
    s2.setValue(s1.getValue());

    inductionVar1.setValue(tmpInduction);
    low1.setValue(tmpLower);
    up1.setValue(tmpUpper);
    s1.setValue(tmpStep);
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

  public static Xnode createVar(String type, String value, Xscope scope,
                            XcodeProgram xcodeml)
  {
    Xnode var = new Xnode(Xcode.VAR, xcodeml);
    var.setAttribute(Xattr.TYPE, type);
    var.setAttribute(Xattr.SCOPE, scope.toString());
    var.setValue(value);
    return var;
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
   * TODO javadoc
   * @param xcodeml
   * @return
   */
  public static Xnode createIfThen(XcodeProgram xcodeml){
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
   * TODO javadoc
   * @param inductionVar
   * @param indexRange
   * @param xcodeml
   * @return
   */
  public static Xnode createDoStmt(Xnode inductionVar,
                                   Xnode indexRange,
                                   XcodeProgram xcodeml)
  {
    Xnode root = new Xnode(Xcode.FDOSTATEMENT, xcodeml);
    root.appendToChildren(inductionVar, false);
    root.appendToChildren(indexRange, false);

    Xnode body = new Xnode(Xcode.BODY, xcodeml);
    root.appendToChildren(body, false);
    return root;
  }

  /**
   * TODO javadoc
   * @param value
   * @param fctCall
   * @return
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
   * TODO javadoc
   * @param xcodeml
   * @param arrayVar
   * @param startIndex
   * @param dimension
   * @return
   */
  public static Xnode createAssumedShapeRange(XcodeProgram xcodeml,
                                              Xnode arrayVar, int startIndex,
                                              int dimension)
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
    fctCall.setAttribute(Xattr.IS_INTRINSIC, XelementName.TRUE);
    fctCall.setAttribute(Xattr.TYPE, XelementName.TYPE_F_INT);
    Xnode name = new Xnode(Xcode.NAME, xcodeml);
    name.setValue(XelementName.INTRINSIC_SIZE);
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
   * TODO javadoc
   * @param xcodeml
   * @return
   */
  public static Xnode createEmptyAssumedShaped(XcodeProgram xcodeml) {
    Xnode range = new Xnode(Xcode.INDEXRANGE, xcodeml);
    range.setAttribute(Xattr.IS_ASSUMED_SHAPE, XelementName.TRUE);
    return range;
  }

  /**
   * TODO javadoc
   * @param opcode
   * @param parent
   * @return
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
   * TODO javadoc
   * @param xcodeml
   * @param returnType
   * @param name
   * @param nameType
   * @return
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
   * TODO javadoc
   * @param type
   * @param var
   * @param xcodeml
   * @return
   */
  public static Xnode createArrayRef(XbasicType type, Xnode var,
                                     XcodeProgram xcodeml)
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
   * @param type      Value for the attribute type.
   * @param sclass    Value for the attribute sclass.
   * @param nameValue Value of the name inner element.
   * @param xcodeml   XcodeML program.
   * @return A newly constructs Xid element with all the information loaded.
   */
  public static Xid createId(String type, String sclass, String nameValue,
                               XcodeProgram xcodeml)
  {
    Xnode id = new Xnode(Xcode.ID, xcodeml);
    Xnode internalName = new Xnode(Xcode.NAME, xcodeml);
    internalName.setValue(nameValue);
    id.appendToChildren(internalName, false);
    id.setAttribute(Xattr.TYPE, type);
    id.setAttribute(Xattr.SCLASS, sclass);
    return new Xid(id.getElement());
  }


}
