/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

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
}
