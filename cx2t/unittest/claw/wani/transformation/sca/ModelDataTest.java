/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import static org.junit.Assert.assertFalse;

import java.util.List;
import java.util.Set;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test features of model data directives.
 *
 * @author clementval
 */
public class ModelDataTest
{

    /**
     * Basic test to gather variable names in model-data directive block.
     */
    @Test
    public void gatherVariableTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_MODEL_DATA1, context);
        assertNotNull(xcodeml);

        List<Xnode> nodes = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
        assertEquals(3, nodes.size());

        Xnode modelDataBegin = nodes.get(0);
        Xnode modelDataEnd = nodes.get(1);

        assertEquals("claw model-data", modelDataBegin.value());
        assertEquals("claw end model-data", modelDataEnd.value());

        Set<String> values = XnodeUtil.getAllVariables(modelDataBegin, modelDataEnd);

        assertEquals(2, values.size());

        assertTrue(values.contains("t"));
        assertTrue(values.contains("q"));
        assertFalse(values.contains("w"));
    }

}
