/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test methods of DimensionDefinition class
 *
 * @author clementval
 */
public class DimensionTest
{

    @Test
    public void dimensionDefinitionTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        DimensionDefinition dimDef = new DimensionDefinition("nproma", "1", "nend");
        DimensionDefinition dimDef2 = new DimensionDefinition("nproma", "nstart", "nend");
        assertEquals("nproma(nstart:nend)", dimDef2.toString());
        assertNotNull(dimDef.getLowerBound());
        assertNotNull(dimDef.getUpperBound());
        assertNotNull(dimDef.getIterationLowerBound());
        assertNotNull(dimDef.getIterationUpperBound());
        assertNotNull(dimDef.getIterationStep());
        assertFalse(dimDef.getLowerBound().isVar());
        assertEquals(1, dimDef.getLowerBound().getIntValue());

        assertFalse(dimDef.getIterationLowerBound().isVar());
        assertEquals(1, dimDef.getIterationLowerBound().getIntValue());

        assertTrue(dimDef.getUpperBound().isVar());
        assertEquals("nend", dimDef.getUpperBound().getValue());

        assertTrue(dimDef.getIterationUpperBound().isVar());
        assertEquals("nend", dimDef.getIterationUpperBound().getValue());

        assertFalse(dimDef.getIterationStep().isVar());
        assertEquals(1, dimDef.getIterationStep().getIntValue());

        assertEquals("nproma", dimDef.getIdentifier());
        assertEquals(InsertionPosition.BEFORE, dimDef.getInsertionPosition());
        dimDef.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(InsertionPosition.AFTER, dimDef.getInsertionPosition());
        assertEquals("nproma(1:nend)", dimDef.toString());

        Xnode indexRange = dimDef.generateIndexRange(xcodeml, false);
        assertNotNull(indexRange);
        assertEquals(Xcode.INDEX_RANGE, indexRange.opcode());
        assertEquals(2, indexRange.children().size());
        assertEquals(Xcode.LOWER_BOUND, indexRange.firstChild().opcode());
        assertEquals(Xcode.UPPER_BOUND, indexRange.lastChild().opcode());

        Xnode indexRangeStep = dimDef.generateIndexRange(xcodeml, true);
        assertNotNull(indexRangeStep);
        assertEquals(Xcode.INDEX_RANGE, indexRangeStep.opcode());
        assertEquals(3, indexRangeStep.children().size());
        assertEquals(Xcode.LOWER_BOUND, indexRangeStep.firstChild().opcode());
        assertEquals(Xcode.UPPER_BOUND, indexRangeStep.child(1).opcode());
        assertEquals(Xcode.STEP, indexRangeStep.lastChild().opcode());

        Xnode arrayIndex = dimDef.generateArrayIndex(xcodeml);
        assertNotNull(arrayIndex);
        assertEquals(Xcode.ARRAY_INDEX, arrayIndex.opcode());
        assertEquals(Xcode.VAR, arrayIndex.firstChild().opcode());
        assertEquals(dimDef.getIdentifier(), arrayIndex.firstChild().value());

        Xnode allocateNode = dimDef.generateAllocateNode(xcodeml);
        assertEquals(Xcode.ARRAY_INDEX, allocateNode.opcode());
        assertEquals(Xcode.VAR, allocateNode.firstChild().opcode());
        assertEquals(dimDef.getUpperBound().getValue(), allocateNode.firstChild().value());
    }

    @Test
    public void dimensionWithIterationDefinitionTest()
    {
        DimensionDefinition dimDef = new DimensionDefinition("block", "1", "nproma", "nstart", "nend", "nstep");

        assertNotNull(dimDef.getLowerBound());
        assertNotNull(dimDef.getUpperBound());
        assertNotNull(dimDef.getIterationLowerBound());
        assertNotNull(dimDef.getIterationUpperBound());
        assertNotNull(dimDef.getIterationStep());

        assertFalse(dimDef.getLowerBound().isVar());
        assertEquals(1, dimDef.getLowerBound().getIntValue());

        assertTrue(dimDef.getIterationLowerBound().isVar());
        assertEquals("nstart", dimDef.getIterationLowerBound().getValue());

        assertTrue(dimDef.getUpperBound().isVar());
        assertEquals("nproma", dimDef.getUpperBound().getValue());

        assertTrue(dimDef.getIterationUpperBound().isVar());
        assertEquals("nend", dimDef.getIterationUpperBound().getValue());

        assertTrue(dimDef.getIterationStep().isVar());
        assertEquals("nstep", dimDef.getIterationStep().getValue());

        assertEquals("block", dimDef.getIdentifier());
    }

    @Test
    public void boundDefinitionTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        BoundDefinition lowerBound = new BoundDefinition("1", BoundDefinition.BoundType.LOWER);
        BoundDefinition upperBound = new BoundDefinition("nend", BoundDefinition.BoundType.LOWER);
        BoundDefinition step = new BoundDefinition("1", BoundDefinition.BoundType.STEP);

        assertFalse(lowerBound.isVar());
        assertTrue(upperBound.isVar());
        assertFalse(step.isVar());

        assertEquals(1, lowerBound.getIntValue());
        assertEquals("nend", upperBound.getValue());
        assertEquals(1, step.getIntValue());

        Xnode lowerNode = lowerBound.generate(xcodeml);
        assertNotNull(lowerNode);
        assertNotNull(lowerNode.firstChild());
        assertEquals(Xcode.F_INT_CONSTANT, lowerNode.firstChild().opcode());
        assertEquals("1", lowerNode.firstChild().value());
        Xnode upperNode = upperBound.generate(xcodeml);
        assertNotNull(upperNode);
        assertNotNull(upperNode.firstChild());
        assertEquals(Xcode.VAR, upperNode.firstChild().opcode());
        assertEquals("nend", upperNode.firstChild().value());
    }
}
