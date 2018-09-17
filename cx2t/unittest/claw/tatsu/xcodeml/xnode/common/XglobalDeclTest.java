/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test the features of the XglobalDecl class.
 *
 * @author clementval
 */
public class XglobalDeclTest {

  private static final String SIMPLE_GLOB_DECL =
      "<globalDeclarations>" +
          "<FmoduleDefinition name=\"module\" lineno=\"4\" " +
          "file=\"./src/module.f90\">" +
          "</FmoduleDefinition>" +
          "<FfunctionDefinition lineno=\"917\" file=\"./src/module.f90\">\n" +
          "<name type=\"Fb39d60\">fct1</name>" +
          "<symbols></symbols>" +
          "<declarations></declarations>" +
          "<body></body>" +
          "</FfunctionDefinition>" +
          "</globalDeclarations>";

  @Test
  public void simpleGlobalDeclarationTest() {
    XglobalDeclTable gdTable =
        XmlHelper.createGlobalDeclTable(SIMPLE_GLOB_DECL);
    assertNotNull(gdTable);
    assertEquals(2, gdTable.size());
    assertTrue(gdTable.hasDefinition("fct1"));
    assertTrue(gdTable.hasFunctionDefinition("fct1"));
    assertFalse(gdTable.hasModuleDefinition("fct1"));
    FfunctionDefinition fDef = gdTable.getFunctionDefinition("fct1");
    assertNotNull(fDef);
    assertEquals("fct1", fDef.getName());
    assertNotNull(fDef.body());
    assertNotNull(fDef.getDeclarationTable());
    assertNull(fDef.getParams());
    assertNotNull(fDef.getSymbolTable());
    assertEquals(917, fDef.lineNo());
    assertEquals("./src/module.f90", fDef.filename());
    assertTrue(gdTable.hasDefinition("module"));
    assertFalse(gdTable.hasFunctionDefinition("module"));
    assertTrue(gdTable.hasModuleDefinition("module"));
    FmoduleDefinition mDef = gdTable.getModuleDefinition("module");
    assertNotNull(mDef);
    assertEquals("module", mDef.getName());
    assertEquals(4, mDef.lineNo());
    assertEquals("./src/module.f90", mDef.filename());
  }
}
