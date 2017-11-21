/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.XfunctionType;
import helper.XmlHelper;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test XfunctionType features
 *
 * @author clementval
 */
public class XfunctionTypeTest {

  private static final String fctType1 = "<FfunctionType type=\"F0\" " +
      "return_type=\"Freal\"><params><name type=\"Fint\">a</name>" +
      "<name type=\"Fint\">b</name></params></FfunctionType>";

  /**
   * Test simple fct type
   * <p>
   * function foo(a,b)
   * integer a, b
   */
  @Test
  public void simpleFctTypeTest() {
    XfunctionType f = XmlHelper.createXfctTypeFromString(fctType1);
    assertFunctionType(f);

    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    XfunctionType emptyFctType =
        new XfunctionType(xcodeml.createNode(Xcode.F_FUNCTION_TYPE));
    assertFalse(emptyFctType.hasParam("a"));

    XfunctionType clone = f.cloneNode();
    assertNotEquals(clone.element(), f.element());
    assertFunctionType(clone);
  }

  private void assertFunctionType(XfunctionType f) {
    assertNotNull(f);

    assertEquals("Freal", f.getReturnType());
    assertNull(f.getResultName());
    assertFalse(f.isRecursive());
    assertFalse(f.isProgram());
    assertFalse(f.isInternal());
    assertFalse(f.isPure());
    assertFalse(f.isElemental());
    assertEquals("F0", f.getType());

    // Test parameters
    assertEquals(2, f.getParameters().size());
    assertEquals("a", f.getParameters().get(0).value());
    assertEquals("Fint", f.getParameters().get(0).getType());
    assertEquals("b", f.getParameters().get(1).value());
    assertEquals("Fint", f.getParameters().get(1).getType());

    List<String> names = f.getParamsNames();
    List<String> expected = Arrays.asList("a", "b");
    assertEquals(expected.size(), names.size());
    for(int i = 0; i < names.size(); ++i) {
      assertEquals(expected.get(i), names.get(i));
    }
    assertTrue(f.hasParam("a"));
    assertFalse(f.hasParam("z"));
  }
}
