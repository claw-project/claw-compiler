/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeML;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test methods of Loop class.
 *
 * @author clementval
 */

public class LoopTest
{

    @Test
    public void mergeFailTest()
    {
        Context context = new TestContext();

        XcodeML xcodeml = XmlHelper.getDummyXcodeProgram(context);
        assertNotNull(xcodeml);

        Xnode n1 = xcodeml.createNode(Xcode.F_IF_STATEMENT);
        Xnode n2 = xcodeml.createNode(Xcode.F_IF_STATEMENT);

        try
        {
            Loop.merge(n1, null);
            fail();
        } catch (IllegalTransformationException ignored)
        {
        }

        try
        {
            Loop.merge(null, n2);
            fail();
        } catch (IllegalTransformationException ignored)
        {
        }

        try
        {
            Loop.merge(n1, n2);
            fail();
        } catch (IllegalTransformationException ignored)
        {
        }
    }

    @Test
    public void mergeTest()
    {
        Context context = new TestContext();
        XcodeML xcodeml = XmlHelper.getDummyXcodeProgram(context);
        assertNotNull(xcodeml);

        DimensionDefinition d1 = new DimensionDefinition("i", "1", "10");

        Xnode inductionI = xcodeml.createVar(FortranType.INTEGER, "i", Xscope.LOCAL);

        Xnode l1 = xcodeml.createDoStmt(inductionI, d1.generateIndexRange(xcodeml, true));
        Xnode l2 = xcodeml.createDoStmt(inductionI, d1.generateIndexRange(xcodeml, true));

<<<<<<< HEAD
        List<FfunctionDefinition> fctDefs = xcodeml.getAllFctDef();
        assertFalse(fctDefs.isEmpty());
=======
    Xnode l1 = xcodeml.createDoStmt(inductionI,
        d1.generateIndexRange(xcodeml, true, false));
    Xnode l2 = xcodeml.createDoStmt(inductionI,
        d1.generateIndexRange(xcodeml, true, false));
>>>>>>> b1ad18a3... Fix for iterations space with automated model-data promotion

        FfunctionDefinition f1 = fctDefs.get(0);

        int doStmtCnt1 = f1.matchAll(Xcode.F_DO_STATEMENT).size();

        f1.body().append(l1);
        f1.body().append(l2);

        int doStmtCnt2 = f1.matchAll(Xcode.F_DO_STATEMENT).size();

        assertEquals(doStmtCnt1 + 2, doStmtCnt2);

        try
        {
            Loop.merge(l1, l2);
        } catch (IllegalTransformationException e)
        {
            fail();
        }

        int doStmtCnt3 = f1.matchAll(Xcode.F_DO_STATEMENT).size();
        assertEquals(doStmtCnt1 + 1, doStmtCnt3);
    }
}
