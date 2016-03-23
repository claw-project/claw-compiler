/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Test the features of the XassignStatement class
 * @author clementval
 */
public class XassignStatementTest {
  private static final String assign1 =
      "<FassignStatement lineno=\"14\" file=\"original_code.f90\">" +
      "<FarrayRef type=\"Fint\">" +
      "<varRef type=\"A7facd1409210\">" +
      "<Var type=\"A7facd1409210\" scope=\"local\">vec1</Var>" +
      "</varRef>" +
      "<arrayIndex>" +
      "<Var type=\"Fint\" scope=\"local\">j</Var>" +
      "</arrayIndex>" +
      "</FarrayRef>" +
      "<Var type=\"Fint\" scope=\"local\">j</Var>" +
      "</FassignStatement>";

  @Test
  public void simpleAssignStatementTest(){
    XassignStatement assignStmt = XmlHelper.createXassignStatement(assign1);
    assertNotNull(assignStmt);
    assertNotNull(assignStmt.getLValueModel());
    assertNotNull(assignStmt.getExprModel());
    assertEquals(14, assignStmt.getLineNo());
    assertEquals("original_code.f90", assignStmt.getFile());

    XLValueModel lhs = assignStmt.getLValueModel();
    assertNotNull(lhs.getElement());
    assertTrue(lhs.isArrayRef());
    assertEquals(XelementName.TYPE_F_INT, lhs.getArrayRef().getType());
    assertNotNull(lhs.getArrayRef().getVarRef());
    assertEquals("A7facd1409210", lhs.getArrayRef().getVarRef().getType());
    assertNotNull(lhs.getArrayRef().getVarRef().getVar());
    Xvar v1 = lhs.getArrayRef().getVarRef().getVar();
    assertEquals("A7facd1409210", v1.getType());
    assertEquals(Xscope.LOCAL, v1.getScope());
    assertEquals("vec1", v1.getValue());
    assertEquals(1, lhs.getArrayRef().getInnerElements().size());
    XbaseElement el1 = lhs.getArrayRef().getInnerElements().get(0);
    assertNotNull(el1);
    assertTrue(XarrayIndex.class.isInstance(el1));
    XarrayIndex ai1 = (XarrayIndex)el1;
    assertTrue(ai1.getExprModel().isVar());
    assertEquals(XelementName.TYPE_F_INT, ai1.getExprModel().getVar().getType());
    assertEquals(Xscope.LOCAL, ai1.getExprModel().getVar().getScope());
    assertEquals("j", ai1.getExprModel().getVar().getValue());


    XexprModel rhs = assignStmt.getExprModel();
    assertNotNull(rhs.getElement());
    assertTrue(rhs.isVar());
    assertEquals(XelementName.TYPE_F_INT, rhs.getVar().getType());
    assertEquals(Xscope.LOCAL, rhs.getVar().getScope());
    assertEquals("j", rhs.getVar().getValue());
  }
}
