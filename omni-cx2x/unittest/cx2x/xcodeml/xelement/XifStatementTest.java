/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test features of the XifStatement, Xcondition, Xthen, Xelse classes.
 *
 * @author clementval
 */
public class XifStatementTest {

  private static final String ifStmt1 =
      "<FifStatement construct_name=\"myif\" lineno=\"3\" file=\"test.f90\">" +
      "<condition>" +
      "<FlogicalConstant type=\"Flogical\">.TRUE.</FlogicalConstant>" +
      "</condition>" +
      "<then>" +
      "<body>" +
      "</body>" +
      "</then>" +
      "<else>" +
      "<body>" +
      "</body>" +
      "</else>" +
      "</FifStatement>";


  @Test
  public void simpleIfStatementTest(){
    XifStatement ifStmt = XmlHelper.createXifStatement(ifStmt1);
    assertNotNull(ifStmt);
    assertNotNull(ifStmt.getConstructName());
    assertEquals("myif", ifStmt.getConstructName());
    assertNotNull(ifStmt.getCondition());
    assertNotNull(ifStmt.getCondition().getExprModel());
    assertTrue(ifStmt.getCondition().getExprModel().isLogicalConst());
    assertNotNull(ifStmt.getCondition().getExprModel().getLogicalConstant());
    assertEquals(".TRUE.",
        ifStmt.getCondition().getExprModel().getLogicalConstant().getValue());
    assertEquals(XelementName.TYPE_F_LOGICAL,
        ifStmt.getCondition().getExprModel().getLogicalConstant().getType());
    assertNotNull(ifStmt.getThen());
    assertNotNull(ifStmt.getThen().getBody());
    assertNotNull(ifStmt.getElse());
    assertNotNull(ifStmt.getElse().getBody());

    assertEquals(3, ifStmt.getLineNo());
    assertEquals("test.f90", ifStmt.getFile());
  }

  @Test
  public void createIfStatementTest(){
    try {
      XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
      XifStatement stmt1 = XifStatement.create(xcodeml);
      assertNotNull(stmt1);
      assertNotNull(stmt1.getCondition());
      assertNotNull(stmt1.getThen());
      assertNotNull(stmt1.getThen().getBody());
      assertNull(stmt1.getElse());

      XifStatement stmt2 = XifStatement.createWithElseBlock(xcodeml);
      assertNotNull(stmt2);
      assertNotNull(stmt2.getCondition());
      assertNotNull(stmt2.getThen());
      assertNotNull(stmt2.getThen().getBody());
      assertNotNull(stmt2.getElse());
      assertNotNull(stmt2.getElse().getBody());
    } catch (IllegalTransformationException e) {
      fail();
    }
  }
}
