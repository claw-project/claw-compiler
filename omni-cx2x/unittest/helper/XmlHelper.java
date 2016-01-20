/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package helper;

import static org.junit.Assert.*;

import cx2x.xcodeml.xelement.*;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.xml.sax.InputSource;

import cx2x.xcodeml.xelement.Xid;
import cx2x.xcodeml.xelement.Xvar;


/**
 * Helper class containing static methods for the unit tests.
 *
 * @author clementval
 */
 
public class XmlHelper {

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

  public static Element getElementFromString(String xml){
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



}
