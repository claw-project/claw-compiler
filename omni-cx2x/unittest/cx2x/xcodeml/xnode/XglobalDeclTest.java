/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test the features of the XglobalDecl class.
 *
 * @author clementval
 */
public class XglobalDeclTest {

  private static final String simpleGlobDecl =
      "<globalDeclarations>" +
      "<FmoduleDefinition name=\"mymodule\" lineno=\"4\" " +
          "file=\"./src/mymodule.f90\">" +
      "</FmoduleDefinition>" +
      "<FfunctionDefinition lineno=\"917\" file=\"./src/mymodule.f90\">\n" +
      "<name type=\"Fb39d60\">fct1</name>" +
      "</FfunctionDefinition>" +
      "</globalDeclarations>";


  @Test
  public void simpleGlobalDeclarationTest(){
    XglobalDeclTable gdTable = XmlHelper.createGlobalDeclTable(simpleGlobDecl);
    assertNotNull(gdTable);
    assertEquals(2, gdTable.count());
    assertTrue(gdTable.hasDefinition("fct1"));
    assertTrue(gdTable.hasFunctionDefinition("fct1"));
    assertFalse(gdTable.hasModuleDefinition("fct1"));
    XfunctionDefinition fDef = gdTable.getFctDefinition("fct1");
    assertNotNull(fDef);
    assertEquals("fct1", fDef.getName().getValue());
    assertNull(fDef.getBody());
    assertNull(fDef.getDeclarationTable());
    assertNull(fDef.getParams());
    assertNull(fDef.getSymbolTable());
    assertEquals(917, fDef.getLineNo());
    assertEquals("./src/mymodule.f90", fDef.getFile());
    assertTrue(gdTable.hasDefinition("mymodule"));
    assertFalse(gdTable.hasFunctionDefinition("mymodule"));
    assertTrue(gdTable.hasModuleDefinition("mymodule"));
    XmoduleDefinition mDef = gdTable.getModuleDefinition("mymodule");
    assertNotNull(mDef);
    assertEquals("mymodule", mDef.getName());
    assertEquals(4, mDef.getLineNo());
    assertEquals("./src/mymodule.f90", mDef.getFile());
  }
}
