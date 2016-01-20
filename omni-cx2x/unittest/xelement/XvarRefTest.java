/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package xelement;

import cx2x.xcodeml.xelement.Xvar;
import cx2x.xcodeml.xelement.XvarRef;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test the features of the XvarRef class
 *
 * @author clementval
 */
public class XvarRefTest {


  private static final String varRefWithVar =
        "<varRef type=\"A7fb02a573b20\">\n"
      + "<Var scope=\"local\" type=\"A7fb02a573b20\">pti_dp</Var>\n"
      + "</varRef>";



  @Test
  public void verRefWithVarTest() {
    XvarRef varRef = XmlHelper.createXvarRefFromString(varRefWithVar);
    assertTrue(varRef.isVar());
    assertEquals("pti_dp", varRef.getVar().getValue());
  }

}
