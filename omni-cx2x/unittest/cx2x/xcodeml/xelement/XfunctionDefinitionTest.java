/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test features of the XfunctionDefinition class.
 *
 * @author clementval
 */
public class XfunctionDefinitionTest {

  private static final String basicFDef =
      "<FfunctionDefinition lineno=\"1\" file=\"original_code.f90\">" +
      "<name type=\"F7ff951406df0\">force_dummy</name>" +
      "<symbols>" +
      "</symbols>" +
      "<declarations>" +
      "</declarations>" +
      "<body>" +
      "<FprintStatement format=\"*\" lineno=\"3\" file=\"original_code.f90\">" +
      "<valueList>" +
      "<value>" +
      "<FcharacterConstant type=\"C7ff9514071e0\">Dummy program for force " +
          "option</FcharacterConstant>" +
      "</value>" +
      "</valueList>" +
      "</FprintStatement>" +
      "</body>" +
      "</FfunctionDefinition>";


  @Test
  public void basicFunctionDefinitionTest(){
    XfunctionDefinition fctDef = XmlHelper.createXfunctionDefinitionFromString(basicFDef);
    assertNotNull(fctDef);
    assertEquals("force_dummy", fctDef.getName().getValue());
    assertEquals(1, fctDef.getLineNo());
    assertEquals("original_code.f90", fctDef.getFile());

  }
}
