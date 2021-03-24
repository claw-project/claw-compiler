/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;

import java.util.List;
import java.util.Set;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test features of the AssignStatement class.
 *
 * @author clementval
 */
public class AssignStatementTest
{

    @Test
    public void gatherAssignmentTest1()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT, context);
        assertNotNull(xcodeml);

        List<Xnode> nodes = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(1, nodes.size());

        assertEquals(Xcode.F_FUNCTION_DEFINITION, nodes.get(0).opcode());
        FfunctionDefinition fctDef = new FfunctionDefinition(nodes.get(0));

        List<AssignStatement> assignStatements = fctDef.gatherAssignStatements();
        assertEquals(2, assignStatements.size());

        assertTrue(assignStatements.get(0).isChildOf(Xcode.F_IF_STATEMENT));
        assertFalse(assignStatements.get(1).isChildOf(Xcode.F_IF_STATEMENT));

        Set<String> vars = assignStatements.get(0).getVarNames();
        assertEquals(2, vars.size());
        assertTrue(vars.contains("t"));
        assertTrue(vars.contains("q"));
    }

    @Test
    public void gatherAssignmentTest2()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT2, context);
        assertNotNull(xcodeml);

        List<Xnode> nodes = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(1, nodes.size());

        assertEquals(Xcode.F_FUNCTION_DEFINITION, nodes.get(0).opcode());
        FfunctionDefinition fctDef = new FfunctionDefinition(nodes.get(0));

        List<AssignStatement> assignStatements = fctDef.gatherAssignStatements();
        assertEquals(4, assignStatements.size());
    }
}
