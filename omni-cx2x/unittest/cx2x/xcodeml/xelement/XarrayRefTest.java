/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import helper.XmlHelper;

/**
 * Test the features of the XarrayRef class
 *
 * @author clementval
 */
public class XarrayRefTest {
  private static final String arrayRef1 =
      "<FarrayRef type=\"Rb0fd40\">" +
      "<varRef type=\"Ab0fe80\">" +
      "<Var type=\"Ab0fe80\" scope=\"local\">array1</Var>" +
      "</varRef>" +
      "<arrayIndex>" +
      "<Var type=\"Ib524e0\" scope=\"local\">i</Var>" +
      "</arrayIndex>" +
      "<arrayIndex>" +
      "<Var type=\"Ib52650\" scope=\"local\">j</Var>" +
      "</arrayIndex>" +
      "</FarrayRef>";

  @Test
  public void simpleArrayRefTest(){
    XarrayRef arrayRef = XmlHelper.createXarrayRef(arrayRef1);
    assertNotNull(arrayRef);
    assertEquals("Rb0fd40", arrayRef.getType());
    assertNotNull(arrayRef.getVarRef());
    assertNotNull(arrayRef.getVarRef().getVar());
    assertEquals("array1", arrayRef.getVarRef().getVar().getValue());
    assertEquals(2, arrayRef.getInnerElements().size());

    XbaseElement el1 = arrayRef.getInnerElements().get(0);
    assertTrue(el1 instanceof XarrayIndex);
    XarrayIndex index1 = (XarrayIndex)el1;
    assertNotNull(index1);
    assertTrue(index1.getExprModel().isVar());
    assertEquals("i", index1.getExprModel().getVar().getValue());

    XbaseElement el2 = arrayRef.getInnerElements().get(1);
    assertTrue(el2 instanceof XarrayIndex);
    XarrayIndex index2 = (XarrayIndex)el2;
    assertNotNull(index2);
    assertTrue(index2.getExprModel().isVar());
    assertEquals("j", index2.getExprModel().getVar().getValue());
  }






}
