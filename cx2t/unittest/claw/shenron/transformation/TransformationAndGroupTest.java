/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.transformation;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static junit.framework.TestCase.fail;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import claw.shenron.translator.AnalyzedPragma;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test transformation group base classes.
 *
 * @author clementval
 */
public class TransformationAndGroupTest
{

    @Test
    public void transformationGroupBaseTest()
    {
        TransformationGroup ig = new IndependentTransformationGroup("loop-hoist");
        TransformationGroup dg = new DependentTransformationGroup("loop-fusion");

        groupTest(dg, "loop-fusion");
        groupTest(ig, "loop-hoist");

        Transformation t1 = new T1();
        ig.add(t1);
        assertEquals(1, ig.count());
        assertFalse(t1.isTransformed());
        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        assertNotNull(xcodeml);
        try
        {
            ig.applyTransformations(xcodeml, null);
        } catch (Exception e)
        {
            fail();
        }
        assertTrue(t1.isTransformed());
    }

    private void groupTest(TransformationGroup tg, String name)
    {
        assertNotNull(tg);
        assertEquals(name, tg.transformationName());
        assertEquals(0, tg.count());
        tg.add(null);
        assertEquals(0, tg.count());
        assertEquals(0, tg.getTransformations().size());
        assertEquals(0, tg.getAppliedTransformationCount());

        try
        {
            tg.applyTransformations(null, null);
            assertEquals(0, tg.getAppliedTransformationCount());
        } catch (Exception ignored)
        {
            fail();
        }

        tg.incrementAppliedTransformation();
        assertEquals(1, tg.getAppliedTransformationCount());
    }

    @Test
    public void dependentTransformationAddingOrderTest()
    {
        Transformation t1 = new T1();
        Transformation t2 = new T1();
        Transformation t3 = new T1();

        t1.setStartLine(3);
        t2.setStartLine(15);
        t3.setStartLine(27);

        TransformationGroup tg = new DependentTransformationGroup("dg");
        tg.add(t3);
        tg.add(t1);
        tg.add(t2);

        assertEquals(3, tg.count());
        assertEquals(t1, tg.getTransformations().get(0));
        assertEquals(t2, tg.getTransformations().get(1));
        assertEquals(t3, tg.getTransformations().get(2));
    }

    @Test
    public void assignTransformationsToGroupTest()
    {
        TransformationGroup ig = new IndependentTransformationGroup("ig");

        List<Transformation> transformations = new ArrayList<>();
        transformations.add(new T1());
        transformations.add(new T2(null, null));

        ig.setTransformations(transformations);

        assertEquals(2, ig.count());
        assertEquals(0, ig.getAppliedTransformationCount());
    }

    @Test
    public void basicTransformationTest()
    {
        Transformation t1 = new T1();
        assertTrue(t1.abortOnFailedAnalysis());
        assertEquals(0, t1.getStartLine());
        t1.setStartLine(10);
        assertEquals(10, t1.getStartLine());
        assertNull(t1.getDirective());
        assertFalse(t1.isTransformed());

        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        Xnode pragma = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
        pragma.setLine(10);
        t1 = new T1(new AnalyzedPragma(pragma));
        assertNotNull(t1.getDirective());
        assertNotNull(t1.getDirective().getPragma());
        assertEquals(10, t1.getStartLine());
    }

    @Test
    public void basicBlockTransformationTest()
    {
        BlockTransformation t2 = new T2(null, null);
        assertTrue(t2.abortOnFailedAnalysis());
        assertEquals(0, t2.getStartLine());
        assertNull(t2.getDirective());
        assertFalse(t2.isTransformed());
        assertNull(t2.getEndDirective());
    }

    /**
     * Only for testing purpose in claw.shenron
     */
    private class T1 extends Transformation
    {

        T1()
        {
            super();
        }

        T1(AnalyzedPragma pragma)
        {
            super(pragma);
        }

        @Override
        public boolean analyze(XcodeProgram xcodeml, Translator translator)
        {
            return true;
        }

        @Override
        public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
        {
            return false;
        }

        @Override
        public void transform(XcodeProgram xcodeml, Translator translator, Transformation other)
        {
            transformed();
        }
    }

    /**
     * Only for testing purpose in claw.shenron
     */
    private class T2 extends BlockTransformation
    {

        T2(AnalyzedPragma startDirective, AnalyzedPragma endDirective)
        {
            super(startDirective, endDirective);
        }

        @Override
        public boolean analyze(XcodeProgram xcodeml, Translator translator)
        {
            return true;
        }

        @Override
        public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
        {
            return false;
        }

        @Override
        public void transform(XcodeProgram xcodeml, Translator translator, Transformation other)
        {
            transformed();
        }
    }
}
