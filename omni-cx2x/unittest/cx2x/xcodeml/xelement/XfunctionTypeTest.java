/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test XfunctionType features
 *
 * @author clementval
 */
public class XfunctionTypeTest {

  private static final String fctType1 =
      "<FfunctionType type=\"F0\" return_type=\"Freal\">" +
      "<params>" +
      "<name type=\"Fint\">a</name>" +
      "<name type=\"Fint\">b</name>" +
      "</params>" +
      "</FfunctionType>";

  /**
   * Test simple fct type
   *
   * function foo(a,b)
   *   integer a, b
   */
  @Test
  public void simpleFctTypeTest(){
    XfunctionType f = XmlHelper.createXfctTypeFromString(fctType1);
    assertNotNull(f);


    assertEquals("Freal", f.getReturnType());
    assertNull(f.getResultName());
    assertFalse(f.isRecursive());
    assertFalse(f.isProgram());
    assertFalse(f.isInternal());
    assertEquals("F0", f.getType());

    // Test parameters
    assertEquals(2, f.getParams().count());
    assertEquals("a", f.getParams().getAll().get(0).getValue());
    assertEquals("Fint", f.getParams().getAll().get(0).getType());
    assertEquals("b", f.getParams().getAll().get(1).getValue());
    assertEquals("Fint", f.getParams().getAll().get(1).getType());
  }

}
