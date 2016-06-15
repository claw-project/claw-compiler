/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package helper;

import static org.junit.Assert.*;

import cx2x.xcodeml.xnode.*;
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

  public static XdeclTable createXdeclTable(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new XdeclTable(el);
  }

  public static Xnode createXpragma(){
    String xml = "<" + Xname.PRAGMA_STMT + "></" +
        Xname.PRAGMA_STMT + ">";
    Element el = XmlHelper.getElementFromString(xml);
    return new Xnode(el);
  }


  public static Xnode createXnode(String xml){
    Element el = XmlHelper.getElementFromString(xml);
    assertNotNull(el);
    return new Xnode(el);
  }
}
