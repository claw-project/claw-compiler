/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;

import java.util.Collections;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test features of the condition primitive class.
 *
 * @author clementval
 */
public class ConditionTest
{

    @Test
    public void dependsOnTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT, context);
        assertNotNull(xcodeml);

        List<Xnode> nodes = xcodeml.matchAll(Xcode.F_IF_STATEMENT);
        assertEquals(1, nodes.size());

        Xnode ifStmt = nodes.get(0);
        assertEquals(Xcode.F_IF_STATEMENT, ifStmt.opcode());

        Xnode condition = ifStmt.matchDirectDescendant(Xcode.CONDITION);
        assertNotNull(condition);

        assertFalse(Condition.dependsOn(condition, Collections.emptySet()));
        assertFalse(Condition.dependsOn(null, Collections.singleton("t")));
        assertFalse(Condition.dependsOn(ifStmt, Collections.singleton("t")));
        assertTrue(Condition.dependsOn(condition, Collections.singleton("t")));
        assertFalse(Condition.dependsOn(condition, Collections.singleton("k")));
    }
}
