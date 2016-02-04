/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test the features of the Xpragma class
 *
 * @author clementval
 */
public class XpragmaTest {

  private static final String pragma1 =
      "<FpragmaStatement lineno=\"9\" file=\"original_code.f90\">" +
      "claw loop-fusion" +
      "</FpragmaStatement>";

  @Test
  public void simplePragmaTest(){
    Xpragma pragma = XmlHelper.createXpragma(pragma1);
    assertNotNull(pragma);
    assertEquals("claw loop-fusion", pragma.getValue());
    assertEquals(9, pragma.getLineNo());
    assertEquals("original_code.f90", pragma.getFile());
  }

  @Test
  public void cloneTest(){
    Xpragma pragma = XmlHelper.createXpragma(pragma1);
    assertNotNull(pragma);
    Xpragma clone = pragma.cloneObject();
    assertEquals("claw loop-fusion", clone.getValue());
    clone.setData("claw loop-interchange");
    assertEquals("claw loop-fusion", pragma.getValue());
    assertEquals("claw loop-interchange", clone.getValue());
    assertEquals(9, pragma.getLineNo());
    assertEquals(9, clone.getLineNo());
    assertEquals("original_code.f90", pragma.getFile());
    assertEquals("original_code.f90", clone.getFile());
  }

}
