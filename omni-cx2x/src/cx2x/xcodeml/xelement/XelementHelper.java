/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.translator.exception.*;

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
    String attrName)
  {
    if(el == null || el.getBaseElement() == null){
      return false;
    }
    String value = XelementHelper.getAttributeValue(el, attrName);
    if(value == null) {
      return false;
    }
    if (value.equals(XelementName.TRUE)){
      return true;
    }
    return false;
  }

  /**
   * Find a function definition according to a function call.
   * @param xcodeml The XcodeML program to search in.
   * @param fctCall The function call used to find the function definition.
   * @return A function definition element if found. Null otherwise.
   */
  public static XfctDef findFunctionDefinition(XcodeProg xcodeml,
    XfctCall fctCall)
  {
    if(xcodeml.getBaseElement() == null){
      return null;
    }
    String name = fctCall.getFctName();
    String type = fctCall.getType();
    NodeList nList = xcodeml.getBaseElement().getElementsByTagName(XelementName.FCT_DEFINITION);
    for (int i = 0; i < nList.getLength(); i++) {
      Node fctDefNode = nList.item(i);
      if (fctDefNode.getNodeType() == Node.ELEMENT_NODE) {
        XbaseElement dummyFctDef = new XbaseElement((Element)fctDefNode);
        Xname fctDefName = findName(dummyFctDef, false);
        if(name != null && fctDefName.isIdentical(name, type)){
          return new XfctDef(dummyFctDef.getBaseElement());
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
  public static List<XarrayRef> getAllArrayReferences(Xbody parent){
    List<XarrayRef> references = new ArrayList<XarrayRef>();
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

  public static List<XrealConstant> getRealConstants(XbaseElement parent){
    List<XrealConstant> elements = new ArrayList<XrealConstant>();
    NodeList nList = parent.getBaseElement().
      getElementsByTagName(XelementName.F_REAL_CONST);
    for (int i = 0; i < nList.getLength(); i++) {
      Node n = nList.item(i);
      if (n.getNodeType() == Node.ELEMENT_NODE) {
        Element el = (Element) n;
        XrealConstant ref = new XrealConstant(el);
        elements.add(ref);
      }
    }
    return elements;
  }


  public static void insertFctCallIntoLoop(XdoStatement loop, XfctCall call){
    loop.getBody().getBaseElement().appendChild(call.getBaseElement().getParentNode());
  }


  public static XfctDef findParentFctDef(XbaseElement child){
    if(child == null || child.getBaseElement() == null){
      return null;
    }
    Node parent = child.getBaseElement().getParentNode();
    while(child.getBaseElement().getParentNode() != null){
      if (parent.getNodeType() == Node.ELEMENT_NODE) {
        Element element = (Element) parent;
        if(element.getTagName().equals(XelementName.FCT_DEFINITION)){
          return new XfctDef(element);
        }
      }
      parent = parent.getParentNode();
    }
    return null;
  }

  public static XdoStatement findLoop(XfctDef fctDef, boolean any){
    Xbody body = fctDef.getBody();
    XdoStatement doStmt = XelementHelper.findDoStatement(body, any);
    return doStmt;
  }

  public static Xvar findVar(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xvar.class);
  }

  public static XvarRef findVarRef(XbaseElement parent, boolean any){
    return findXelement(parent, any, XvarRef.class);
  }

  public static XindexRange findIndexRange(XbaseElement parent, boolean any){
    return findXelement(parent, any, XindexRange.class);
  }

  public static XarrayIndex findArrayIndex(XbaseElement parent, boolean any){
    return findXelement(parent, any, XarrayIndex.class);
  }

  public static Xname findName(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xname.class);
  }

  public static Xvalue findValue(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xvalue.class);
  }

  public static XexprModel findExprModel(XbaseElement parent){
    /** An exprModel can be of the following type
     *   TODO
     *   - FintConstant, FrealConstant, FcomplexConstant, FcharacterConstant,
     *     FlogicalConstant
     *   TODO
     *   - FarrayConstructor, FstructConstructor
     *   - Var
     *   TODO
     *   - FarrayRef, FcharacterRef, FmemberRef, FcoArrayRef, varRef
     *   - functionCall
     *   TODO
     *   - plusExpr, minusExpr, mulExpr, divExpr, FpowerExpr, FconcatExpr
     *     logEQExpr, logNEQExpr, logGEExpr, logGTExpr, logLEExpr, logLTExpr,
     *     logAndExpr, logOrExpr, logEQVExpr, logNEQVExpr, logNotExpr,
     *     unaryMinusExpr, userBinaryExpr, userUnaryExpr
     *   TODO
     *   - FdoLoop
     */

    // Try to find constant
    Xconstant constant = findConstant(parent, false);
    if(constant != null){
      return new XexprModel(constant);
    }

    // Try to find var
    Xvar var = findVar(parent, false);
    if(var != null){
      return new XexprModel(var);
    }

    // TODO all findFunction here msut be perform on direct children only
    // otherwise it will fails ... big refactoring needed


    // Try to find fctCall
    XfctCall fctCall = findFctCall(parent, false);
    if(fctCall != null) {
      return new XexprModel(fctCall);
    }


    return null; // TODO
  }

  /**
   *
   * @return A Xconstant object if one is found. null otherwise.
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

  public static XintConstant findIntConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XintConstant.class);
  }

  public static XrealConstant findRealConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XrealConstant.class);
  }

  public static XcomplexConstant findComplexConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XcomplexConstant.class);
  }

  public static XcharacterConstant findCharacterConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XcharacterConstant.class);
  }

  public static XlogicalConstant findLogicalConstant(XbaseElement parent, boolean any){
    return findXelement(parent, any, XlogicalConstant.class);
  }

  public static Xthen findThen(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xthen.class);
  }

  public static Xelse findElse(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xelse.class);
  }

  public static XargumentsTable findArgumentsTable(XbaseElement parent, boolean any){
    return findXelement(parent, any, XargumentsTable.class);
  }

  public static XlowerBound findLowerBound(XbaseElement parent, boolean any){
    return findXelement(parent, any, XlowerBound.class);
  }

  public static XupperBound findUpperBound(XbaseElement parent, boolean any){
    return findXelement(parent, any, XupperBound.class);
  }

  public static Xstep findStep(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xstep.class);
  }

  public static Xbody findBody(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xbody.class);
  }

  public static XdoStatement findDoStatement(XbaseElement parent, boolean any){
    return findXelement(parent, any, XdoStatement.class);
  }

  public static XdoStatement findNextDoStatement(XbaseElement from){
    return findNextElementOfType(from, XdoStatement.class);
  }

  public static XsymbolTable findSymbols(XbaseElement parent, boolean any){
    return findXelement(parent, any, XsymbolTable.class);
  }

  public static XdeclTable findDeclarations(XbaseElement parent, boolean any){
    return findXelement(parent, any, XdeclTable.class);
  }

  public static XtypeTable findTypeTable(XcodeProg parent, boolean any){
    return findXelement(parent, any, XtypeTable.class);
  }

  public static XglobalSymbolTable findGlobalSymbols(XcodeProg parent,
    boolean any)
  {
    return findXelement(parent, any, XglobalSymbolTable.class);
  }

  public static int findNumberOfRange(XbaseElement parent){
    NodeList elements = parent.getBaseElement().
      getElementsByTagName(XelementName.INDEX_RANGE);
    return elements.getLength();
  }

  public static ArrayList<XindexRange> findIndexRanges(XbaseElement parent){
    ArrayList<XindexRange> indexRanges = new ArrayList<XindexRange>();
    if(parent == null || parent.getBaseElement() == null){
      return indexRanges;
    }
    NodeList ranges = parent.getBaseElement().getElementsByTagName(XelementName.INDEX_RANGE);
    for(int i = 0; i < ranges.getLength(); ++i){
      indexRanges.add(new XindexRange((Element)ranges.item(i)));
    }
    return indexRanges;
  }

  public static Xlength findLen(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xlength.class);
  }

  public static Xkind findKind(XbaseElement parent, boolean any){
    return findXelement(parent, any, Xkind.class);
  }

  public static XfctCall findFctCall(XbaseElement parent, boolean any){
    return findXelement(parent, any, XfctCall.class);
  }

  /**
   * Find a function call element nested in the given expression statement.
   * @param exprStmt The expression statement to search from.
   * @return A function call element if found. Null otherwise.
   */
  public static XfctCall findFctCall(XexprStatement exprStmt){
    if(exprStmt == null){
      return null;
    }

    NodeList nodeList = exprStmt.getBaseElement().getChildNodes();
    for (int i = 0; i < nodeList.getLength(); i++) {
      Node nextNode = nodeList.item(i);
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(XelementName.FCT_CALL)){
          return new XfctCall(element);
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
   * Find if there is a if statement directly after the given element.
   * @param from  The element to search from
   * @return An if statement element if found. Null otherwise.
   */
  public static XifStatement findDirectNextIfStmt(XbaseElement from){
    return findDirectNextElement(from, XifStatement.class);
  }


  /**
   * Delete all the elements between the two given pragmas.
   * @param start The start pragma. Deletion start from next element.
   * @param end   The end pragma. Deletion end just before this element.
   */
  public static void deleteBetween(Xpragma start, Xpragma end){
    ArrayList<Element> toDelete = new ArrayList<Element>();
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
  public static List<Xpragma> findAllPragmas(XcodeProg xcodeml){
    NodeList pragmaList = xcodeml.getDocument()
      .getElementsByTagName(XelementName.PRAGMA_STMT);
    List<Xpragma> pragmas = new ArrayList<Xpragma>();
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

    if(e1.getBaseElement().getParentNode()
      == e2.getBaseElement().getParentNode())
    {
      return true;
    }
    return false;
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
    for(Node childNode = body.getFirstChild(); childNode!=null;){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        XelementHelper.insertAfter(refNode, (Element)childNode);
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
    element.getParentNode().removeChild(element);
  }


  /**
   * Write the XcodeML to file or std out
   * @param xcodeml    The XcodeML to write in the output
   * @param outputFile Path of the output file or null to output on std out
   * @param indent     Number of spaces used for the indentation
   * @return true if the output could be write without problems.
   */
  public static boolean writeXcodeML(XcodeProg xcodeml, String outputFile, int indent) {
    try {
      XelementHelper.cleanEmptyTextNodes(xcodeml.getDocument());
      Transformer transformer
        = TransformerFactory.newInstance().newTransformer();
      transformer.setOutputProperty(OutputKeys.INDENT, "yes");
      transformer.setOutputProperty(
                "{http://xml.apache.org/xslt}indent-amount",
                Integer.toString(2));
      DOMSource source = new DOMSource(xcodeml.getDocument());
      if(outputFile == null){
        // Output to console
        StreamResult console = new StreamResult(System.out);
      } else {
        // Output to file
        StreamResult console = new StreamResult(new File(outputFile));
        transformer.transform(source, console);
      }
    } catch (TransformerConfigurationException ex){
      // TODO move to stderr and correct error msg
      System.out.println("Cannot output file: " + ex.getMessage());
      return false;
    } catch (TransformerException ex){
      // TODO move to stderr and correct error msg
      System.out.println("Cannot output file: " + ex.getMessage());
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

  public static boolean validateStringAttribute(Document doc, String attrValue
    , String xpathQuery) throws Exception
  {
    XPathFactory xPathfactory = XPathFactory.newInstance();
    XPath xpath = xPathfactory.newXPath();
    XPathExpression getVersion = xpath.compile(xpathQuery);
    String outputValue = (String) getVersion.evaluate(doc,
      XPathConstants.STRING);
    if(outputValue.equals(attrValue)){
      return true;
    }
    return false;
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

    if (nodeType == Node.ELEMENT_NODE
        || nodeType == Node.CDATA_SECTION_NODE
        || nodeType == Node.COMMENT_NODE) {
      return true;
    } else {
      return false;
    }
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
        T xelement = xElementClass.
          getDeclaredConstructor(Element.class).newInstance(element);
        return xelement;
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
          try{
            T xelement = xElementClass.
              getDeclaredConstructor(Element.class).newInstance(element);
            return xelement;
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
            T xelement = xElementClass.
              getDeclaredConstructor(Element.class).newInstance(element);
            return xelement;
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

}
