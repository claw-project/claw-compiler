/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package helper;

import static org.junit.Assert.*;

import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.xnode.Xnode;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import java.io.File;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.InputSource;


/**
 * Helper class containing static methods for the unit tests.
 *
 * @author clementval
 */
 
public class XmlHelper {

  public static XcodeProgram getDummyXcodeProgram(){
    File f = new File(TestConstant.TEST_DATA);
    assertTrue(f.exists());
    XcodeProgram xcodeml =  XcodeProgram.createFromFile(TestConstant.TEST_DATA);
    assertNotNull(xcodeml);
    return xcodeml;
  }

  public static Document loadXMLFromString(String xml) {
    try {
      DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
      DocumentBuilder builder = factory.newDocumentBuilder();
      InputSource is = new InputSource(new StringReader(xml));
      return builder.parse(is);
    } catch(Exception ex){
      return null;
    }
  }

  private static Element getElementFromString(String xml){
    Document doc = loadXMLFromString(xml);
    if(doc != null){
      return doc.getDocumentElement();
    }
    return null;
  }

  public static Xid createXidFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xid(el);
  }

  public static Xvar createXvarFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xvar(el);
  }

  public static XvarRef createXvarRefFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XvarRef(el);
  }

  public static XbasicType createXbasicTypeFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XbasicType(el);
  }

  public static XfunctionType createXfctTypeFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XfunctionType(el);
  }

  public static XglobalSymbolTable createXglobalSymbolFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XglobalSymbolTable(el);
  }

  public static XsymbolTable createXSymbolTableFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XsymbolTable(el);
  }

  public static XtypeTable createXtypeTableFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XtypeTable(el);
  }

  public static XfunctionDefinition createXfunctionDefinitionFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XfunctionDefinition(el);
  }

  public static XfunctionCall createXfunctionCallFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XfunctionCall(el);
  }

  public static XintConstant createIntConstantFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XintConstant(el);
  }

  public static XrealConstant createRealConstantFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XrealConstant(el);
  }

  public static XlogicalConstant createLogicalConstantFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XlogicalConstant(el);
  }

  public static XcharacterConstant createCharConstantFromString(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XcharacterConstant(el);
  }

  public static XglobalDeclTable createGlobalDeclTable(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XglobalDeclTable(el);
  }

  public static XvarDecl createXvarDecl(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XvarDecl(el);
  }

  public static XarrayRef createXarrayRef(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XarrayRef(el);
  }

  public static XassignStatement createXassignStatement(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XassignStatement(el);
  }

  public static XdoStatement createXdoStatement(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XdoStatement(el);
  }

  public static XmoduleDefinition createXmoduleDefinition(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XmoduleDefinition(el);
  }

  public static Xpragma createXpragma(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xpragma(el);
  }

  public static XifStatement createXifStatement(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XifStatement(el);
  }

  public static XindexRange createXindexRange(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XindexRange(el);
  }

  public static Xvalue createXvalue(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xvalue(el);
  }

  public static Xname createXname(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xname(el);
  }

  public static XdeclTable createXdeclTable(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XdeclTable(el);
  }

  public static XcomplexConstant createXcomplexConstant(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XcomplexConstant(el);
  }

  public static Xbody createXbody(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xbody(el);
  }

  public static XloopIterationRange createXloopIterationRange(String var,
                                                              String indexRange)
  {
    Element el = XmlHelper.getElementFromString(var);
    assertNotNull(el);
    Xvar xvar = new Xvar(el);
    el = XmlHelper.getElementFromString(indexRange);
    assertNotNull(el);
    XindexRange xindexRange = new XindexRange(el);

    return new XloopIterationRange(xvar, xindexRange);
  }

  public static Xpragma createXpragma(){
    String xml = "<" + XelementName.PRAGMA_STMT + "></" +
        XelementName.PRAGMA_STMT + ">";
    Element el = XmlHelper.getElementFromString(xml);
    return new Xpragma(el);
  }


  public static Xnode createXnode(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xnode(el);
  }
}
