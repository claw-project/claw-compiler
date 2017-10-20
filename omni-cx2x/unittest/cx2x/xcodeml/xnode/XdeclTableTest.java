/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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

  private static final String decl1 = "<declarations>" +
      "<varDecl lineno=\"4730\" file=\"dummy.f90\">" +
      "<name type=\"I1241bd0\">name1</name></varDecl>\n" +
      "<varDecl lineno=\"4731\" file=\"dummy.f90\">" +
      "<name type=\"I1241c70\">name2</name></varDecl>" +
      "</declarations>";

  @Test
  public void simpleDeclTableTest() {
    XdeclTable decl = XmlHelper.createXdeclTable(decl1);
    assertNotNull(decl);
    assertEquals(2, decl.count());

    Xdecl var1 = decl.get("name1");
    assertNotNull(var1);
    assertEquals(4730, var1.lineNo());
    assertEquals("dummy.f90", var1.filename());
    assertEquals("name1", var1.matchSeq(Xcode.NAME).value());
    assertEquals("I1241bd0", var1.getType());

    Xdecl var2 = decl.get("name2");
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

    List<Xnode> modules = xcodeml.matchAll(Xcode.FMODULEDEFINITION);
    assertEquals(1, modules.size());

    XmoduleDefinition mod = new XmoduleDefinition(modules.get(0));
    XdeclTable modDecl = mod.getDeclarationTable();
    assertNotNull(modDecl);
    List<Xdecl> modDeclarations = modDecl.values();
    assertEquals(2, modDeclarations.size());
    assertEquals(Xcode.FSTRUCTDECL, modDeclarations.get(0).opcode());
    assertEquals(Xcode.FINTERFACEDECL, modDeclarations.get(1).opcode());

    Xdecl interface1 = modDecl.get("dummy");
    assertNotNull(interface1);
    assertEquals(Xcode.FINTERFACEDECL, interface1.opcode());

    assertEquals(1, modDecl.values(Xcode.FINTERFACEDECL).size());

    List<Xnode> functions = xcodeml.matchAll(Xcode.FFUNCTIONDEFINITION);
    assertEquals(1, functions.size());

    XfunctionDefinition fctDef = new XfunctionDefinition(functions.get(0));
    XdeclTable fctDecl = fctDef.getDeclarationTable();
    assertNotNull(fctDecl);
    List<Xdecl> fctDeclarations = fctDecl.values();
    assertEquals(10, fctDeclarations.size());
    assertEquals(Xcode.VARDECL, fctDeclarations.get(0).opcode());
    assertEquals(Xcode.VARDECL, fctDeclarations.get(1).opcode());
    assertEquals(Xcode.FNAMELISTDECL, fctDeclarations.get(2).opcode());
    assertEquals(Xcode.VARDECL, fctDeclarations.get(3).opcode());
    assertEquals(Xcode.VARDECL, fctDeclarations.get(4).opcode());
    assertEquals(Xcode.FCOMMONDECL, fctDeclarations.get(5).opcode());
    assertEquals(Xcode.FUSEDECL, fctDeclarations.get(6).opcode());
    assertEquals(Xcode.FUSEONLYDECL, fctDeclarations.get(7).opcode());
    assertEquals(Xcode.FEQUIVALENCEDECL, fctDeclarations.get(8).opcode());
    assertEquals(Xcode.EXTERNDECL, fctDeclarations.get(9).opcode());

    Xdecl varDecl1 = fctDecl.get("sub1");
    assertNotNull(varDecl1);
    assertEquals(Xcode.VARDECL, varDecl1.opcode());

    Xdecl namelist = fctDecl.get("case");
    assertNotNull(namelist);
    assertEquals(Xcode.FNAMELISTDECL, namelist.opcode());

    Xdecl useDecl = fctDecl.get("mod4");
    assertNotNull(useDecl);
    assertEquals(Xcode.FUSEDECL, useDecl.opcode());

    Xdecl useOnlyDecl = fctDecl.get("mod5");
    assertNotNull(useOnlyDecl);
    assertEquals(Xcode.FUSEONLYDECL, useOnlyDecl.opcode());

    Xdecl externDecl = fctDecl.get("interface_sub");
    assertNotNull(externDecl);
    assertEquals(Xcode.EXTERNDECL, externDecl.opcode());

    assertEquals(4, fctDecl.values(Xcode.VARDECL).size());
    assertEquals(1, fctDecl.values(Xcode.FUSEDECL).size());
    assertEquals(1, fctDecl.values(Xcode.FUSEONLYDECL).size());
    assertEquals(1, fctDecl.values(Xcode.FNAMELISTDECL).size());
    assertEquals(1, fctDecl.values(Xcode.FEQUIVALENCEDECL).size());
    assertEquals(1, fctDecl.values(Xcode.EXTERNDECL).size());
  }
}
