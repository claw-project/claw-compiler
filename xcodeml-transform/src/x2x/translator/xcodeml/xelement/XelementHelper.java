package x2x.translator.xcodeml.xelement;

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

public class XelementHelper {




  public static String getAttributeValue(Element el, String attrName){
    NamedNodeMap attributes = el.getAttributes();
    for (int j = 0; j < attributes.getLength(); j++) {
      if(attributes.item(j).getNodeName().equals(attrName)){
        return attributes.item(j).getNodeValue();
      }
    }
    return null;
  }

  public static boolean getBooleanAttributeValue(Element el, String attrName) {
    String value = XelementHelper.getAttributeValue(el, attrName);
    if(value == null) {
      return false;
    }
    if (value.equals(XelementName.TRUE)){
      return true;
    }
    return false;
  }

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
        Element fctDefElement = (Element) fctDefNode;
        Xname fctDefName = findName(fctDefElement);
        if(name != null && fctDefName.isIdentical(name, type)){
          return new XfctDef(fctDefElement);
        }
      }
    }
    return null;
  }

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

  public static XdoStatement findLoop(XfctDef fctDef){
    Xbody body = fctDef.getBody();
    XdoStatement doStmt = XelementHelper.findLoopStament(body);
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

  public static Xname findName(XbaseElement parent){
    return findName(parent.getBaseElement());
  }
  public static Xname findName(Element parent){ // TODO to be removed
    Element element = findFirstElement(parent, XelementName.NAME);
    return (element != null) ? new Xname(element) : null;
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
    XfctCall fctCall = findFctCall(parent);
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

  public static <T extends XbaseElement> T findXelement(XbaseElement parent,
    boolean any, Class<T> xElementClass)
  {
    String elementName = XelementName.getElementNameFromClass(xElementClass);
    Element element = findElement(parent, elementName, any);
    if (element != null){
      try{
        T xelement = xElementClass.getDeclaredConstructor(Element.class).newInstance(element);
        return xelement;
      } catch(Exception ex){
        return null;
      }
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

  public static XdoStatement findLoopStament(XbaseElement parent){
    Element element = findFirstElement(parent, XelementName.DO_STMT);
    return (element != null) ? new XdoStatement(element) : null;
  }

  public static XsymbolTable findSymbols(XbaseElement parent, boolean any){
    return findXelement(parent, any, XsymbolTable.class);
  }

  public static XdeclTable findDeclarations(XbaseElement parent, boolean any){
    return findXelement(parent, any, XdeclTable.class);
  }

  public static XtypeTable findTypeTable(Document doc){
    NodeList elements = doc.getElementsByTagName(XelementName.TYPE_TABLE);
    Element element = (Element) elements.item(0);
    return (element != null) ? new XtypeTable(element) : null;
  }

  public static XsymbolTable findGlobalSymbols(Document doc){
    NodeList elements = doc.getElementsByTagName(XelementName.GLOBAL_SYMBOLS);
    Element element = (Element) elements.item(0);
    return (element != null) ? new XsymbolTable(element) : null;
  }

  public static int findNumberOfRange(Element parent){
    NodeList elements = parent.getElementsByTagName(XelementName.INDEX_RANGE);
    return elements.getLength();
  }

  public static NodeList findIndexRanges(Element parent){
    return parent.getElementsByTagName(XelementName.INDEX_RANGE);
  }

  public static Xlength findLen(XbaseElement parent){
    Element element = findFirstElement(parent, XelementName.LENGTH);
    return (element != null) ? new Xlength(element) : null;
  }

  public static Xkind findKind(XbaseElement parent){
    Element element = findFirstElement(parent, XelementName.KIND);
    return (element != null) ? new Xkind(element) : null;
  }

  public static void insertAfter(Node refNode, Node newNode){
    refNode.getParentNode().insertBefore(newNode, refNode.getNextSibling());
  }

  public static void insertBefore(XbaseElement ref, XbaseElement insert){
    ref.getBaseElement().getParentNode().insertBefore(insert.getBaseElement(),
      ref.getBaseElement());
  }

  public static void insertAfter(XbaseElement refElement, XbaseElement element){
    XelementHelper.insertAfter(refElement.getBaseElement(),
      element.getBaseElement());
  }

  public static XdoStatement findChildLoop(Xbody from){
    Node nextNode = from.getBaseElement().getFirstChild();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals("FdoStatement")){
          return new XdoStatement(element);
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  public static XfctCall findFctCall(XbaseElement parent){
    Element element = findFirstElement(parent, XelementName.FCT_CALL);
    return (element != null) ? new XfctCall(element) : null;
  }

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


  public static XdoStatement findNextLoop(XbaseElement from){
    Element loopElement = findNextElementOfType(from, XelementName.DO_STMT);
    return (loopElement == null) ? null : new XdoStatement(loopElement);
  }

  public static XexprStatement findNextExprStatement(XbaseElement from){
    Element element = findNextElementOfType(from, XelementName.EXPR_STMT);
    return (element == null) ? null : new XexprStatement(element);
  }

  private static Element findNextElementOfType(XbaseElement from, String tag){
    if(from == null || from.getBaseElement() == null){
      return null;
    }
    Node nextNode = from.getBaseElement().getNextSibling();
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(tag)){
          return element;
        }
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

  public static XdoStatement findDirectNextDoStmt(Node from){
    Element el = findDirectNextElement(from, XelementName.DO_STMT);
    if(el == null){
      return null;
    }
    return new XdoStatement(el);
  }

  public static XifStatement findDirectNextIfStmt(Node from){
    Element el = findDirectNextElement(from, XelementName.F_IF_STMT);
    if(el == null){
      return null;
    }
    return new XifStatement(el);
  }

  private static Element findDirectNextElement(Node from, String tag){
    Node nextNode = from.getNextSibling();
    boolean elementFound = false;
    while (nextNode != null){
      if(nextNode.getNodeType() == Node.ELEMENT_NODE){
        Element element = (Element) nextNode;
        if(element.getTagName().equals(tag)){
          return element;
        }
        return null;
      }
      nextNode = nextNode.getNextSibling();
    }
    return null;
  }

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

  public static void appendBody(Element elementMaster, Element elementSlave) {
    Element masterBody = XelementHelper.findFirstElement(elementMaster,
      XelementName.BODY);
    Element slaveBody = XelementHelper.findFirstElement(elementSlave,
      XelementName.BODY);
    // Append content of loop-body (loop) to this loop-body
    for(Node childNode = slaveBody.getFirstChild(); childNode!=null;){
      Node nextChild = childNode.getNextSibling();
      // Do something with childNode, including move or delete...
      if(childNode.getNodeType() == Node.ELEMENT_NODE){
        masterBody.appendChild(childNode);
      }
      childNode = nextChild;
    }
  }

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
   * CDATA child - COMMENT child
   *
   * The purpose of this is to make the format() method (that use a
   * Transformer for formatting) more consistent regarding indenting and line
   * breaks.
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
   * PRIVATE SECTION
   */

   // TODO description
  private static Element findElement(XbaseElement parent, String elementName, boolean any){
    return findElement(parent.getBaseElement(), elementName, any);
  }

  /**
   * Find an element starting from the parent.
   *
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

  private static Element findFirstElement(Element parent, String elementName){
    NodeList elements = parent.getElementsByTagName(elementName);
    if(elements.getLength() == 0){
      return null;
    }
    return (Element) elements.item(0);
  }

  /**
   * Search only in the direct children of the parent element and not in the
   * children of children TODO
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
