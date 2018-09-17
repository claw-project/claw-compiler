/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import helper.TestConstant;
import helper.XmlHelper;
import org.junit.Test;

import java.io.File;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test the features of the XdeclTable class.
 *
 * @author clementval
 */
public class XdeclTableTest {

  private static final String DECL1 = "<declarations>" +
      "<varDecl lineno=\"4730\" file=\"dummy.f90\">" +
      "<name type=\"I1241bd0\">name1</name></varDecl>\n" +
      "<varDecl lineno=\"4731\" file=\"dummy.f90\">" +
      "<name type=\"I1241c70\">name2</name></varDecl>" +
      "</declarations>";

  @Test
  public void simpleDeclTableTest() {
    XdeclTable decl = XmlHelper.createXdeclTable(DECL1);
    assertNotNull(decl);
    assertEquals(2, decl.count());

    Xnode var1 = decl.get("name1");
    assertNotNull(var1);
    assertEquals(4730, var1.lineNo());
    assertEquals("dummy.f90", var1.filename());
    assertEquals("name1", var1.matchSeq(Xcode.NAME).value());
    assertEquals("I1241bd0", var1.getType());

    Xnode var2 = decl.get("name2");
    assertNotNull(var2);
    assertEquals(4731, var2.lineNo());
    assertEquals("dummy.f90", var2.filename());
    assertEquals("name2", var2.matchSeq(Xcode.NAME).value());
    assertEquals("I1241c70", var2.getType());
  }

  @Test
  public void allKindOfDeclTest() {
    File f = new File(TestConstant.TEST_PROGRAM);
    assertTrue(f.exists());
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_DECLARATIONS);
    assertNotNull(xcodeml);

    XglobalDeclTable global = xcodeml.getGlobalDeclarationsTable();
    assertNotNull(global);
    assertNull(global.getFunctionDefinition("unknown"));
    assertNull(global.getModuleDefinition("unknown"));

    List<Xnode> modules = xcodeml.matchAll(Xcode.F_MODULE_DEFINITION);
    assertEquals(1, modules.size());

    FmoduleDefinition mod = new FmoduleDefinition(modules.get(0));
    XdeclTable modDecl = mod.getDeclarationTable();
    assertNotNull(modDecl);
    List<Xnode> modDeclarations = modDecl.values();
    assertEquals(2, modDeclarations.size());
    assertEquals(Xcode.F_STRUCT_DECL, modDeclarations.get(0).opcode());
    assertEquals(Xcode.F_INTERFACE_DECL, modDeclarations.get(1).opcode());

    Xnode interface1 = modDecl.get("dummy");
    assertNotNull(interface1);
    assertEquals(Xcode.F_INTERFACE_DECL, interface1.opcode());

    assertEquals(1, modDecl.values(Xcode.F_INTERFACE_DECL).size());

    List<Xnode> functions = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
    assertEquals(1, functions.size());

    FfunctionDefinition fctDef = new FfunctionDefinition(functions.get(0));
    XdeclTable fctDecl = fctDef.getDeclarationTable();
    assertNotNull(fctDecl);
    List<Xnode> fctDeclarations = fctDecl.values();
    assertEquals(10, fctDeclarations.size());
    assertEquals(Xcode.VAR_DECL, fctDeclarations.get(0).opcode());
    assertEquals(Xcode.VAR_DECL, fctDeclarations.get(1).opcode());
    assertEquals(Xcode.F_NAMELIST_DECL, fctDeclarations.get(2).opcode());
    assertEquals(Xcode.VAR_DECL, fctDeclarations.get(3).opcode());
    assertEquals(Xcode.VAR_DECL, fctDeclarations.get(4).opcode());
    assertEquals(Xcode.F_COMMON_DECL, fctDeclarations.get(5).opcode());
    assertEquals(Xcode.F_USE_DECL, fctDeclarations.get(6).opcode());
    assertEquals(Xcode.F_USE_ONLY_DECL, fctDeclarations.get(7).opcode());
    assertEquals(Xcode.F_EQUIVALENCE_DECL, fctDeclarations.get(8).opcode());
    assertEquals(Xcode.EXTERN_DECL, fctDeclarations.get(9).opcode());

    Xnode varDecl1 = fctDecl.get("sub1");
    assertNotNull(varDecl1);
    assertEquals(Xcode.VAR_DECL, varDecl1.opcode());

    Xnode namelist = fctDecl.get("case");
    assertNotNull(namelist);
    assertEquals(Xcode.F_NAMELIST_DECL, namelist.opcode());

    Xnode useDecl = fctDecl.get("mod4");
    assertNotNull(useDecl);
    assertEquals(Xcode.F_USE_DECL, useDecl.opcode());

    Xnode useOnlyDecl = fctDecl.get("mod5");
    assertNotNull(useOnlyDecl);
    assertEquals(Xcode.F_USE_ONLY_DECL, useOnlyDecl.opcode());

    Xnode externDecl = fctDecl.get("interface_sub");
    assertNotNull(externDecl);
    assertEquals(Xcode.EXTERN_DECL, externDecl.opcode());

    assertEquals(4, fctDecl.values(Xcode.VAR_DECL).size());
    assertEquals(1, fctDecl.values(Xcode.F_USE_DECL).size());
    assertEquals(1, fctDecl.values(Xcode.F_USE_ONLY_DECL).size());
    assertEquals(1, fctDecl.values(Xcode.F_NAMELIST_DECL).size());
    assertEquals(1, fctDecl.values(Xcode.F_EQUIVALENCE_DECL).size());
    assertEquals(1, fctDecl.values(Xcode.EXTERN_DECL).size());
  }
}
