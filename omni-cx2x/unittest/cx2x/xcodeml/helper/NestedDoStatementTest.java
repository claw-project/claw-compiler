/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.helper;

import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * Test class NestedDoStatement
 */
public class NestedDoStatementTest {

  //"<FdoStatement><body></body></FdoStatement>";

  private static final String group1 = "<FdoStatement>" +
      "<body></body>" +
      "</FdoStatement>";

  private static final String group2 = "<FdoStatement>" +
      "<body><FdoStatement><body></body></FdoStatement></body>" +
      "</FdoStatement>";

  private static final String group3 = "<FdoStatement>" +
      "<body>" +
      "<FdoStatement>" +
      "<body>" +
      "<FdoStatement>" +
      "<body></body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>";

  @Test
  public void ctorTest() {
    // One do statement
    Xnode do1 = XmlHelper.createXnode(group1);
    assertNotNull(do1);
    NestedDoStatement ndo1 = new NestedDoStatement(do1);
    assertNotNull(ndo1);
    assertEquals(1, ndo1.size());
    assertEquals(ndo1.getOuterStatement(), ndo1.getInnerStatement());

    // Two do statements
    Xnode do2 = XmlHelper.createXnode(group2);
    assertNotNull(do2);
    NestedDoStatement ndo2 = new NestedDoStatement(do2);
    assertNotNull(ndo2);
    assertEquals(2, ndo2.size());
    assertEquals(ndo2.get(0), ndo2.getOuterStatement());
    assertEquals(ndo2.get(1), ndo2.getInnerStatement());
    assertNotEquals(ndo2.getOuterStatement(), ndo2.getInnerStatement());

    // Two do statements
    Xnode do3 = XmlHelper.createXnode(group3);
    assertNotNull(do3);
    NestedDoStatement ndo3 = new NestedDoStatement(do3);
    assertNotNull(ndo3);
    assertEquals(3, ndo3.size());
    assertEquals(ndo3.get(0), ndo3.getOuterStatement());
    assertEquals(ndo3.get(2), ndo3.getInnerStatement());
    assertNotEquals(ndo3.getOuterStatement(), ndo3.getInnerStatement());

    // Only two do statements in a potiential 3 nested group one
    NestedDoStatement ndo3_only2 = new NestedDoStatement(do3, 2);
    assertNotNull(ndo3_only2);
    assertEquals(2, ndo3_only2.size());
    assertEquals(ndo3_only2.get(0), ndo3_only2.getOuterStatement());
    assertEquals(ndo3_only2.get(1), ndo3_only2.getInnerStatement());
    List<Xnode> doStmts = do3.matchAll(Xcode.FDOSTATEMENT);
    assertEquals(2, doStmts.size());
  }
}
