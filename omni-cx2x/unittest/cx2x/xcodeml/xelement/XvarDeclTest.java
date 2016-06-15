/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.xnode.Xattr;
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
      "<varDecl lineno=\"946\" file=\"./src/mymodule.f90\">" +
      "<name type=\"Ib3f750\">testvar</name>" +
      "<value>10.0</value>" +
      "</varDecl>";


  private static final String varDecl2 =
      "<varDecl lineno=\"946\" file=\"./src/mymodule.f90\">" +
      "<name type=\"Ib3f750\">testvar</name>" +
      "</varDecl>";

  @Test
  public void simpleXvarDeclWithValueTest(){
    XvarDecl varDecl = XmlHelper.createXvarDecl(varDecl1);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.getLineNo());
    assertEquals("./src/mymodule.f90", varDecl.getFile());
    assertNotNull(varDecl.getName());
    assertEquals("Ib3f750", varDecl.getName().getAttribute(Xattr.TYPE));
    assertEquals("testvar", varDecl.getName().getValue());
    assertTrue(varDecl.hasValue());
    assertEquals("10.0", varDecl.getValue());
  }

  @Test
  public void simpleXvarDeclWithoutValueTest(){
    XvarDecl varDecl = XmlHelper.createXvarDecl(varDecl2);
    assertNotNull(varDecl);
    assertEquals(946, varDecl.getLineNo());
    assertEquals("./src/mymodule.f90", varDecl.getFile());
    assertNotNull(varDecl.getName());
    assertEquals("Ib3f750", varDecl.getName().getAttribute(Xattr.TYPE));
    assertEquals("testvar", varDecl.getName().getValue());
    assertFalse(varDecl.hasValue());
    assertNull(varDecl.getValue());
  }



}
