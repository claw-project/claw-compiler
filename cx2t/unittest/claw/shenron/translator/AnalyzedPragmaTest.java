/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.translator;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test methods of the AnalyzedPragma class.
 *
 * @author clementval
 */
public class AnalyzedPragmaTest
{

    @Test
    public void ctorTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        Xnode p1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
        p1.setValue("acc parallel");
        Xnode p2 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
        p2.setValue("acc end parallel");

        AnalyzedPragma ap1 = new AnalyzedPragma(p1);
        assertFalse(ap1.isEndPragma());
        assertNotNull(ap1.getPragma());
        assertEquals("acc parallel", ap1.getPragma().value());

        ap1.setEndPragma();
        assertTrue(ap1.isEndPragma());

        AnalyzedPragma ap2 = new AnalyzedPragma();
        assertFalse(ap2.isEndPragma());
        assertNull(ap2.getPragma());

        ap2.setPragma(p2);
        ap2.setEndPragma();
        assertTrue(ap2.isEndPragma());
        assertNotNull(ap2.getPragma());
    }
}
