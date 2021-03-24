/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.TestConstant;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test features of the FmoduleDefinition class.
 *
 * @author clementval
 */
public class FmoduleDefinitionTest
{

    private static final String module1 = "<FmoduleDefinition name=\"module\" "
            + "lineno=\"4\" file=\"./src/module.f90\"></FmoduleDefinition>";

    @Test
    public void simpleModuleDefinitionTest()
    {
        Context context = new TestContext();
        Xnode node = XmlHelper.createXnode(module1);
        FmoduleDefinition mod = new FmoduleDefinition(node);
        assertNotNull(mod);
        assertEquals("module", mod.getName());
        assertEquals(4, mod.lineNo());
        assertEquals("./src/module.f90", mod.filename());
        assertNull(mod.getSymbolTable());
        assertNull(mod.getDeclarationTable());

        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DECLARATIONS, context);
        assertNotNull(xcodeml);
        List<Xnode> nodes = xcodeml.matchAll(Xcode.F_MODULE_DEFINITION);
        assertFalse(nodes.isEmpty());
        FmoduleDefinition modDef = new FmoduleDefinition(nodes.get(0));
        assertEquals("mod1", modDef.getName());
        assertFalse(modDef.getFunctionDefinition(null).isPresent());
        assertFalse(modDef.getFunctionDefinition("").isPresent());
        assertTrue(modDef.getFunctionDefinition("sub1").isPresent());

        assertNotNull(modDef.cloneNode());
    }
}
