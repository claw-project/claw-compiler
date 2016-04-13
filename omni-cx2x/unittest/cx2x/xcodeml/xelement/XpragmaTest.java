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

  private static final String pragma2 =
      "<FpragmaStatement lineno=\"9\" file=\"original_code.f90\">" +
      "acc parallel" +
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
  public void appendTest(){
    Xpragma pragma = XmlHelper.createXpragma(pragma2);
    assertNotNull(pragma);
    assertEquals("acc parallel", pragma.getValue());
    assertEquals(9, pragma.getLineNo());
    assertEquals("original_code.f90", pragma.getFile());
    pragma.append("private(var1)");
    assertEquals("acc parallel private(var1)", pragma.getValue());
  }

  @Test
  public void cloneTest(){
    Xpragma pragma = XmlHelper.createXpragma(pragma1);
    assertNotNull(pragma);
    Xpragma clone = pragma.cloneObject();
    assertEquals("claw loop-fusion", clone.getValue());
    clone.setValue("claw loop-interchange");
    assertEquals("claw loop-fusion", pragma.getValue());
    assertEquals("claw loop-interchange", clone.getValue());
    assertEquals(9, pragma.getLineNo());
    assertEquals(9, clone.getLineNo());
    assertEquals("original_code.f90", pragma.getFile());
    assertEquals("original_code.f90", clone.getFile());
  }

}
