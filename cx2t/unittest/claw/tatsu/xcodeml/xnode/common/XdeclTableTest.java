/*

 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Files;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FmoduleDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import helper.TestConstant;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test the features of the XdeclTable class.
 *
 * @author clementval
 */
public class XdeclTableTest
{

    private static final String DECL1 = "<declarations>" + "<varDecl lineno=\"4730\" file=\"dummy.f90\">"
            + "<name type=\"I1241bd0\">name1</name></varDecl>\n" + "<varDecl lineno=\"4731\" file=\"dummy.f90\">"
            + "<name type=\"I1241c70\">name2</name></varDecl>" + "</declarations>";

    @Test
    public void emptyTest()
    {
        XdeclTable decl = XmlHelper.createXdeclTable("<declarations></declarations>");
        assertNotNull(decl);
        assertEquals(0, decl.count());
        assertTrue(decl.uses().isEmpty());
        assertTrue(decl.values().isEmpty());
        assertFalse(decl.contains("name1"));
        assertNull(decl.get("name1"));
    }

    @Test
    public void simpleDeclTableTest()
    {
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

    private XcodeProgram createFromFile(Context context)
    {
        assertTrue(Files.exists(TestConstant.TEST_DECLARATIONS));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DECLARATIONS, context);
        assertNotNull(xcodeml);
        return xcodeml;
    }

    @Test
    public void allKindOfDeclTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = createFromFile(context);

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

    @Test
    public void addRemoveTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = createFromFile(context);

        List<Xnode> functions = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(1, functions.size());
        FfunctionDefinition fctDef = new FfunctionDefinition(functions.get(0));
        assertNotNull(fctDef);

        XdeclTable fctDecl = fctDef.getDeclarationTable();
        assertNotNull(fctDecl);

        String key1 = "key1";
        String key2 = "key2";
        String key3 = "key3";

        // Add
        Xnode varDecl1 = xcodeml.createVarDecl(xcodeml.getTypeTable().generateHash(FortranType.INTEGER), key1);
        fctDecl.add(varDecl1);
        assertNotNull(fctDecl.get(key1));

        // Replace existing one
        Xnode varDecl2 = xcodeml.createVarDecl(xcodeml.getTypeTable().generateHash(FortranType.INTEGER), key1);
        fctDecl.replace(varDecl2, key1);
        assertNotNull(fctDecl.get(key1));
        assertEquals(varDecl2.getType(), fctDecl.get(key1).getType());

        // Replace non-existing - like add
        Xnode varDecl3 = xcodeml.createVarDecl(xcodeml.getTypeTable().generateHash(FortranType.INTEGER), key2);
        fctDecl.replace(varDecl3, key2);
        assertNotNull(fctDecl.get(key2));
        assertEquals(varDecl3.getType(), fctDecl.get(key2).getType());

        // Add at the beginning of the table
        Xnode varDecl4 = xcodeml.createVarDecl(xcodeml.getTypeTable().generateHash(FortranType.INTEGER), key3);
        fctDecl.addFirst(varDecl4);
        assertEquals(varDecl4.getType(), fctDecl.firstChild().getType());

    }

}
