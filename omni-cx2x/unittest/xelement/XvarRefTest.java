/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package xelement;

import cx2x.xcodeml.xelement.*;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test the features of the XvarRef class
 *
 * @author clementval
 */
public class XvarRefTest {


  private static final String varRefWithVar =
      "<varRef type=\"A7fb02a573b20\">\n" +
      "<Var scope=\"local\" type=\"A7fb02a573b20\">pti_dp</Var>\n" +
      "</varRef>";


  private static final String varRefWithArrayRef =
      "<varRef type=\"A7fb02a573b20\">\n" +
      "<FarrayRef type=\"R7fb02a5ad8f0\">\n" +
      "<varRef type=\"A7fb02a5ada10\">\n" +
      "<Var scope=\"local\" type=\"A7fb02a5ada10\">pflt</Var>\n" +
      "</varRef>\n" +
      "<arrayIndex>\n" +
      "<Var scope=\"local\" type=\"I7fb02a76e700\">ip</Var>\n" +
      "</arrayIndex>\n" +
      "<arrayIndex>\n" +
      "<Var scope=\"local\" type=\"I7fb02a76e860\">k</Var>\n" +
      "</arrayIndex>\n" +
      "</FarrayRef>" +
      "</varRef>";



  @Test
  public void varRefWithVarTest() {
    XvarRef varRef = XmlHelper.createXvarRefFromString(varRefWithVar);
    assertNotNull(varRef.getVar());
    assertTrue(varRef.isVar());
    assertEquals("pti_dp", varRef.getVar().getValue());
    assertEquals(Xscope.LOCAL, varRef.getVar().getScope());
  }

  @Test
  public void varRefWithArrayRefTest() {
    XvarRef varRef = XmlHelper.createXvarRefFromString(varRefWithArrayRef);
    assertNotNull(varRef.getArrayRef());
    assertTrue(varRef.isArrayRef());
    assertTrue(varRef.getArrayRef().getVarRef().isVar());
    assertNotNull(varRef.getArrayRef().getVarRef().getVar());
    assertEquals("pflt", varRef.getArrayRef().getVarRef().getVar().getValue());
    assertEquals(Xscope.LOCAL,
        varRef.getArrayRef().getVarRef().getVar().getScope());

  }

}
