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

  private static final String varDecl1 =
      "<varDecl lineno=\"946\" file=\"./src/module.f90\">" +
          "<name type=\"Ib3f750\">testVar</name>" +
          "<value>10.0</value>" +
          "</varDecl>";


  private static final String varDecl2 =
      "<varDecl lineno=\"946\" file=\"./src/module.f90\">" +
          "<name type=\"Ib3f750\">testVar</name>" +
          "</varDecl>";

  @Test
  public void simpleXvarDeclWithValueTest() {
    Xdecl varDecl = XmlHelper.createXvarDecl(varDecl1);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.getLineNo());
    assertEquals("./src/module.f90", varDecl.getFile());
    assertNotNull(varDecl.find(Xcode.NAME));
    assertEquals("Ib3f750", varDecl.find(Xcode.NAME).getAttribute(Xattr.TYPE));
    assertEquals("testVar", varDecl.find(Xcode.NAME).getValue());
    assertNotNull(varDecl.find(Xcode.VALUE));
    assertEquals("10.0", varDecl.find(Xcode.VALUE).getValue());
  }

  @Test
  public void simpleXvarDeclWithoutValueTest() {
    Xdecl varDecl = XmlHelper.createXvarDecl(varDecl2);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.getLineNo());
    assertEquals("./src/module.f90", varDecl.getFile());
    assertNotNull(varDecl.find(Xcode.NAME));
    assertEquals("Ib3f750", varDecl.find(Xcode.NAME).getAttribute(Xattr.TYPE));
    assertEquals("testVar", varDecl.find(Xcode.NAME).getValue());
    assertNull(varDecl.find(Xcode.VALUE));
  }


}
