/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import helper.XmlHelper;
import org.junit.Test;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import static org.junit.Assert.*;


/**
 * Test features of Xbody class
 *
 * @author clementval
 */
public class XbodyTest {

  @Test
  public void appendAsFirstEmptyBodyTest(){
    try {
      XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
      Xpragma p = XelementHelper.createEmpty(Xpragma.class, xcodeml);
      XifStatement ifStmt = XifStatement.create(xcodeml);
      ifStmt.getThen().getBody().appendAsFirst(p);

      Node n = ifStmt.getThen().getBody().getBaseElement().getFirstChild();
      assertTrue(n instanceof Element);
      assertEquals(XelementName.PRAGMA_STMT, ((Element)n).getTagName());
    } catch (IllegalTransformationException ex){
      fail();
    }
  }

  @Test
  public void appendAsFirstNotEmptyBodyTest(){
    try {
      XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
      Xpragma p1 = XelementHelper.createEmpty(Xpragma.class, xcodeml);
      p1.setValue("pragma1");
      Xpragma p2 = XelementHelper.createEmpty(Xpragma.class, xcodeml);
      p2.setValue("pragma2");
      XifStatement ifStmt = XifStatement.create(xcodeml);
      ifStmt.getThen().getBody().appendToChildren(p1, false);
      ifStmt.getThen().getBody().appendAsFirst(p2);
      Node n = ifStmt.getThen().getBody().getBaseElement().getFirstChild();
      assertTrue(n instanceof Element);
      assertEquals(XelementName.PRAGMA_STMT, ((Element)n).getTagName());
      assertEquals("pragma2", ((Element)n).getTextContent());
    } catch (IllegalTransformationException ex){
      fail();
    }
  }
}
