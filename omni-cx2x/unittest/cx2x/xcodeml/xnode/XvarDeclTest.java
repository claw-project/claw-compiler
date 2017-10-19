/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test features of XvarDecl class
 *
 * @author clementval
 */
public class XvarDeclTest {

  private static final String varDecl1 = "<varDecl lineno=\"946\" " +
      "file=\"./src/module.f90\"><name type=\"Ib3f750\">testVar</name>" +
      "<value>10.0</value></varDecl>";
  private static final String varDecl2 = "<varDecl lineno=\"946\" " +
      "file=\"./src/module.f90\"><name type=\"Ib3f750\">testVar</name>" +
      "</varDecl>";

  @Test
  public void simpleXvarDeclWithValueTest() {
    Xdecl varDecl = XmlHelper.createXvarDecl(varDecl1);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.lineNo());
    assertEquals("./src/module.f90", varDecl.filename());
    assertNotNull(varDecl.matchSeq(Xcode.NAME));
    assertEquals("Ib3f750", varDecl.getType());
    assertEquals("testVar", varDecl.matchSeq(Xcode.NAME).value());
    assertNotNull(varDecl.matchSeq(Xcode.VALUE));
    assertEquals("10.0", varDecl.matchSeq(Xcode.VALUE).value());
  }

  @Test
  public void simpleXvarDeclWithoutValueTest() {
    Xdecl varDecl = XmlHelper.createXvarDecl(varDecl2);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.lineNo());
    assertEquals("./src/module.f90", varDecl.filename());
    assertNotNull(varDecl.matchSeq(Xcode.NAME));
    assertEquals("Ib3f750", varDecl.getType());
    assertEquals("testVar", varDecl.matchSeq(Xcode.NAME).value());
    assertNull(varDecl.matchSeq(Xcode.VALUE));
  }


}
