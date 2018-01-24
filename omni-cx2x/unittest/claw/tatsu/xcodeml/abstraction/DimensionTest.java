/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.*;

/**
 * Test methods of DimensionDefinition class
 *
 * @author clementval
 */
public class DimensionTest {

  @Test
  public void dimensionDefinitionTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    DimensionDefinition dimDef = new DimensionDefinition("nproma", "1", "nend");
    DimensionDefinition dimDef2
        = new DimensionDefinition("nproma", "nstart", "nend");
    assertEquals("nproma(nstart:nend)", dimDef2.toString());
    assertNotNull(dimDef.getLowerBound());
    assertNotNull(dimDef.getUpperBound());
    assertFalse(dimDef.getLowerBound().isVar());
    assertEquals(1, dimDef.getLowerBound().getIntValue());
    assertTrue(dimDef.getUpperBound().isVar());
    assertEquals("nend", dimDef.getUpperBound().getValue());
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
    assertEquals(dimDef.getUpperBound().getValue(),
        allocateNode.firstChild().value());
  }

  @Test
  public void boundDefinitionTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    BoundDefinition lowerBound =
        new BoundDefinition("1", BoundDefinition.BoundType.LOWER);
    BoundDefinition upperBound =
        new BoundDefinition("nend", BoundDefinition.BoundType.LOWER);

    assertFalse(lowerBound.isVar());
    assertTrue(upperBound.isVar());

    assertEquals(1, lowerBound.getIntValue());
    assertEquals("nend", upperBound.getValue());

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
