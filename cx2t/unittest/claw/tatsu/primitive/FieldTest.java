/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.abstraction.BoundDefinition;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test methods of Field class.
 *
 * @author clementval
 */
public class FieldTest
{

    @Test
    public void readFromFormattedDimensionTest()
    {
        DimensionDefinition dim1 = new DimensionDefinition("dim1", "1", "30");
        DimensionDefinition dim2 = new DimensionDefinition("dim2", "1", "nproma");
        PromotionInfo p1 = new PromotionInfo("a");

        p1.readDimensionsFromString("dim1(1:30),:");
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        assertEquals(1, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));

        p1.readDimensionsFromString(":,dim1(1:30)");
        dim1.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(1, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));

        p1.readDimensionsFromString(":,dim1(1:30),:");
        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals(1, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));

        p1.readDimensionsFromString("dim1(1:30),dim2(1:nproma),:");
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.BEFORE);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));

        p1.readDimensionsFromString(":,dim1(1:30),dim2(1:nproma),:");
        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));

        p1.readDimensionsFromString(":,dim1(1:30),dim2(1:nproma)");
        dim1.setInsertionPosition(InsertionPosition.AFTER);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));

        p1.readDimensionsFromString("dim1(1:30),:,dim2(1:nproma)");
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));

        p1.readDimensionsFromString("dim1(1:30),:,dim2(1:nproma),:");
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));

        p1.readDimensionsFromString(":,dim1(1:30),:,dim2(1:nproma)");
        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(2, p1.getDimensions().size());
        assertDimensionEquals(dim1, p1.getDimensions().get(0));
        assertDimensionEquals(dim2, p1.getDimensions().get(1));
    }

    private void assertDimensionEquals(DimensionDefinition d1, DimensionDefinition d2)
    {
        assertEquals(d1.getIdentifier(), d2.getIdentifier());
        assertBoundEquals(d1.getLowerBound(), d2.getLowerBound());
        assertBoundEquals(d1.getUpperBound(), d2.getUpperBound());
        assertEquals(d1.getInsertionPosition(), d2.getInsertionPosition());
    }

    private void assertBoundEquals(BoundDefinition b1, BoundDefinition b2)
    {
        assertEquals(b1.isVar(), b2.isVar());
        if (b1.isVar())
        {
            assertEquals(b1.getValue(), b2.getValue());
        } else
        {
            assertEquals(b1.getIntValue(), b2.getIntValue());
        }
    }

    @Test
    public void formattedDimensionsTest()
    {
        DimensionDefinition dim1 = new DimensionDefinition("dim1", "1", "30");
        DimensionDefinition dim2 = new DimensionDefinition("dim2", "1", "40");
        List<DimensionDefinition> dimensions1 = Collections.singletonList(dim1);
        List<DimensionDefinition> dimensions2 = Arrays.asList(dim1, dim2);

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        PromotionInfo p1 = new PromotionInfo("a", dimensions1);
        assertEquals("dim1(1:30),:", p1.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals(":,dim1(1:30),:", p1.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(":,dim1(1:30)", p1.getFormattedDimensions());

        PromotionInfo p2 = new PromotionInfo("a", dimensions2);

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.BEFORE);
        assertEquals("dim1(1:30),dim2(1:40),:", p2.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(":,dim1(1:30),dim2(1:40)", p2.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals(":,dim1(1:30),dim2(1:40),:", p2.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals("dim1(1:30),:,dim2(1:40)", p2.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        assertEquals(":,dim1(1:30),:,dim2(1:40)", p2.getFormattedDimensions());

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        assertEquals("dim1(1:30),:,dim2(1:40),:", p2.getFormattedDimensions());

        PromotionInfo p3 = new PromotionInfo("c");
        assertEquals("", p3.getFormattedDimensions());
    }

    @Test
    public void promoteTest()
    {
        DimensionDefinition dim1 = new DimensionDefinition("dim1", "1", "30");
        DimensionDefinition dim2 = new DimensionDefinition("dim2", "1", "40");

        List<DimensionDefinition> dimensions1 = Collections.singletonList(dim1);
        List<DimensionDefinition> dimensions2 = Arrays.asList(dim1, dim2);

        Context context = new TestContext();

        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_PROMOTION, context);
        assertNotNull(xcodeml);

        List<FfunctionDefinition> fctDefs = xcodeml.getAllFctDef();
        assertEquals(1, fctDefs.size());

        FfunctionDefinition fctDef = fctDefs.get(0);
        assertEquals("sub1", fctDef.getName());

        // Scalar to array promotion with 1 additional dimension
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        performAndAssertPromotion("s1", dimensions1, fctDef, xcodeml, 0, 1, new int[] { 1, 30 });

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        performAndAssertPromotion("s2", dimensions1, fctDef, xcodeml, 0, 1, new int[] { 1, 30 });

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("s3", dimensions1, fctDef, xcodeml, 0, 1, new int[] { 1, 30 });

        // Scalar to array promotion with 2 additional dimension
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.BEFORE);
        performAndAssertPromotion("s4", dimensions2, fctDef, xcodeml, 0, 2, new int[] { 1, 30, 1, 40 });

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        performAndAssertPromotion("s5", dimensions2, fctDef, xcodeml, 0, 2, new int[] { 1, 30, 1, 40 });

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("s6", dimensions2, fctDef, xcodeml, 0, 2, new int[] { 1, 30, 1, 40 });

        // Promotion with 1 additional dimension
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        performAndAssertPromotion("a", dimensions1, fctDef, xcodeml, 2, 3, new int[] { 1, 30, 1, 10, 1, 20 });

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("b", dimensions1, fctDef, xcodeml, 2, 3, new int[] { 1, 10, 1, 20, 1, 30 });

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        performAndAssertPromotion("c", dimensions1, fctDef, xcodeml, 2, 3, new int[] { 1, 10, 1, 30, 1, 20 });

        // Promotion with 2 additional dimensions at the same position
        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.BEFORE);
        performAndAssertPromotion("d", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 30, 1, 40, 1, 10, 1, 20 });

        dim1.setInsertionPosition(InsertionPosition.AFTER);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("e", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 10, 1, 20, 1, 30, 1, 40 });

        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        performAndAssertPromotion("f", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 10, 1, 30, 1, 40, 1, 20 });

        // Promotion with 2 additional dimensions at different position
        dim1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("g", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 10, 1, 30, 1, 20, 1, 40 });

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
        performAndAssertPromotion("h", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 30, 1, 10, 1, 40, 1, 20 });

        dim1.setInsertionPosition(InsertionPosition.BEFORE);
        dim2.setInsertionPosition(InsertionPosition.AFTER);
        performAndAssertPromotion("i", dimensions2, fctDef, xcodeml, 2, 4, new int[] { 1, 30, 1, 10, 1, 20, 1, 40 });
    }

    /**
     * Perform the promotion transformation and assert its result.
     *
     * @param id         Identifier of the field to be promoted.
     * @param dims       Dimension definitions to use.
     * @param fctDef     Function definition in which the promotion is performed.
     * @param xcodeml    Current XcodeML translation unit.
     * @param base       Number of dimension before the promotion.
     * @param target     Number of dimension after the promotion.
     * @param dimensions Expected dimensions after promotion.
     */
    private void performAndAssertPromotion(String id, List<DimensionDefinition> dims, FfunctionDefinition fctDef,
            XcodeProgram xcodeml, int base, int target, int[] dimensions)
    {
        try
        {

            int diff = target - base;

            PromotionInfo promotionInfo = new PromotionInfo(id);
            promotionInfo.setDimensions(dims);

            Xnode decl = fctDef.getDeclarationTable().get(id);
            assertNotNull(decl);
            FbasicType bt;
            if (base != 0)
            {
                bt = xcodeml.getTypeTable().getBasicType(decl);
                assertNotNull(bt);
                assertTrue(bt.isArray());
                assertEquals(base, bt.getDimensions());
            } else
            {
                assertEquals(FortranType.REAL, FortranType.fromString(decl.getType()));
            }

            // Perform the promotion
            Field.promote(promotionInfo, fctDef, xcodeml);
            decl = fctDef.getDeclarationTable().get(id);
            assertNotNull(decl);
            bt = xcodeml.getTypeTable().getBasicType(decl);
            assertNotNull(bt);
            assertTrue(bt.isArray());
            assertEquals(target, bt.getDimensions());
            assertEquals(target, dimensions.length / 2);
            assertEquals(target, promotionInfo.getTargetDimension());
            assertEquals(base, promotionInfo.getBaseDimension());
            assertEquals(diff, promotionInfo.diffDimension());
            assertEquals(bt.getType(), promotionInfo.getTargetType().getType());

            if (base > 0)
            {
                assertEquals(PromotionInfo.PromotionType.ARRAY_TO_ARRAY, promotionInfo.getPromotionType());
            } else
            {
                assertEquals(PromotionInfo.PromotionType.SCALAR_TO_ARRAY, promotionInfo.getPromotionType());
            }

            for (int i = 0; i < dimensions.length / 2; ++i)
            {
                assertDimension(bt.getDimensions(i), dimensions[i * 2], dimensions[(i * 2) + 1]);
            }
        } catch (IllegalTransformationException itex)
        {
            System.err.println(itex.getMessage());
            fail();
        }
    }

    /**
     * Assert information of a dimension.
     *
     * @param dimension  Node representing the dimension (indexRange node).
     * @param lowerBound Value of the lowerBound.
     * @param upperBound Value of the upperBound.
     */
    private void assertDimension(Xnode dimension, int lowerBound, int upperBound)
    {
        assertNotNull(dimension);
        assertEquals(Xcode.INDEX_RANGE, dimension.opcode());
        Xnode lowerBoundNode = dimension.firstChild();
        assertNotNull(lowerBoundNode);
        Xnode upperBoundNode = dimension.lastChild();
        assertNotNull(upperBoundNode);
        assertEquals(Xcode.F_INT_CONSTANT, lowerBoundNode.firstChild().opcode());
        assertEquals(String.valueOf(lowerBound), lowerBoundNode.firstChild().value());
        assertEquals(Xcode.F_INT_CONSTANT, upperBoundNode.firstChild().opcode());
        assertEquals(String.valueOf(upperBound), upperBoundNode.firstChild().value());
    }
}
