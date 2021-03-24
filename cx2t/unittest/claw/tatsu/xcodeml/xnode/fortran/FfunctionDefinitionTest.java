/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.nio.file.Files;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import helper.TestConstant;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test features of the FfunctionDefinition class.
 *
 * @author clementval
 */
public class FfunctionDefinitionTest
{

    private static final String basicFDef = "<FfunctionDefinition lineno=\"1\" " + "file=\"original_code.f90\">"
            + "<name type=\"F7ff951406df0\">force_dummy</name>" + "<symbols>" + "</symbols>" + "<declarations>"
            + "</declarations>" + "<body>" + "<FprintStatement format=\"*\" lineno=\"3\" file=\"original_code.f90\">"
            + "<valueList>" + "<value>" + "<FcharacterConstant type=\"C7ff9514071e0\">Dummy program for force "
            + "option</FcharacterConstant>" + "</value>" + "</valueList>" + "</FprintStatement>" + "</body>"
            + "</FfunctionDefinition>";

    @Test
    public void basicFunctionDefinitionTest()
    {
        Context context = new TestContext();
        context.init(CompilerDirective.OPENACC, Target.GPU, null, 80);
        FfunctionDefinition fctDef = XmlHelper.createXfunctionDefinitionFromString(basicFDef);
        assertNotNull(fctDef);
        assertEquals("force_dummy", fctDef.getName());
        assertEquals("force_dummy", fctDef.name().value());
        assertEquals("F7ff951406df0", fctDef.getType());
        assertEquals(0, fctDef.getSymbolTable().size());
        assertEquals(0, fctDef.getDeclarationTable().count());
        assertNull(fctDef.getParams());
        assertNotNull(fctDef.body());

        assertEquals(1, fctDef.lineNo());
        assertEquals("original_code.f90", fctDef.filename());

        assertNull(fctDef.findContainingXmod(context));

        FfunctionDefinition clone = fctDef.cloneNode();
        assertNotNull(clone);
        assertEquals("force_dummy", fctDef.getName());
        assertEquals("force_dummy", fctDef.name().value());
    }

    @Test
    public void findContainingXmodTest()
    {
        Context context = new TestContext();
        context.init(CompilerDirective.OPENACC, Target.GPU, null, 80);
        assertTrue(Files.exists(TestConstant.TEST_DECLARATIONS));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DECLARATIONS, context);
        assertNotNull(xcodeml);
        List<FfunctionDefinition> fctDefs = xcodeml.getAllFctDef();
        for (FfunctionDefinition fctDef : fctDefs)
        {
            // Search paths is not set so module cannot be found.
            assertNull(fctDef.findContainingXmod(context));
        }
    }
}
