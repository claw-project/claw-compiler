/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import helper.TestConstant;
import helper.XmlHelper;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.*;

/**
 * Test features of the FmoduleDefinition class.
 *
 * @author clementval
 */
public class FmoduleDefinitionTest {

  private static final String module1 = "<FmoduleDefinition name=\"module\" " +
      "lineno=\"4\" file=\"./src/module.f90\"></FmoduleDefinition>";

  @Test
  public void simpleModuleDefinitionTest() {
    Xnode node = XmlHelper.createXnode(module1);
    FmoduleDefinition mod = new FmoduleDefinition(node);
    assertNotNull(mod);
    assertEquals("module", mod.getName());
    assertEquals(4, mod.lineNo());
    assertEquals("./src/module.f90", mod.filename());
    assertNull(mod.getSymbolTable());
    assertNull(mod.getDeclarationTable());

    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_DECLARATIONS);
    assertNotNull(xcodeml);
    List<Xnode> nodes = xcodeml.matchAll(Xcode.F_MODULE_DEFINITION);
    assertTrue(!nodes.isEmpty());
    FmoduleDefinition modDef = new FmoduleDefinition(nodes.get(0));
    assertEquals("mod1", modDef.getName());
    assertNull(modDef.getFunctionDefinition(null));
    assertNull(modDef.getFunctionDefinition(""));
    assertNotNull(modDef.getFunctionDefinition("sub1"));

    assertNotNull(modDef.cloneNode());
  }
}
