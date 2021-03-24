/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test methods from the XcodeML class.
 *
 * @author clementval
 */
public class XcodeMLTest
{

    @Test
    public void createPragmaTest()
    {
        String shortPragma = "acc loop seq";
        String longPragma = "acc data present(var1,var2,var3,var4,var5,var6,var7,"
                + "var8,var9,var10,var11,var12,var13,var14,var15,var16,var17,var18,"
                + "var19,var20,var21,var22,var23,var24,var25,var26,var27,var28,var29," + "var30)";

        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);

        List<Xnode> p1 = xcodeml.createPragma(null, 80);
        assertNotNull(p1);
        assertTrue(p1.isEmpty());

        List<Xnode> p2 = xcodeml.createPragma("", 80);
        assertNotNull(p2);
        assertTrue(p2.isEmpty());

        List<Xnode> p3 = xcodeml.createPragma(shortPragma, 80);
        assertNotNull(p3);
        assertEquals(1, p3.size());
        assertEquals(shortPragma, p3.get(0).value());

        List<Xnode> p4 = xcodeml.createPragma(longPragma, 80);
        assertNotNull(p4);
        assertEquals(4, p4.size());
        assertEquals("acc data &", p4.get(0).value());
        assertEquals("acc present(var1,var2,var3,var4,var5,var6,var7,var8,var9," + "var10,var11,var12 &",
                p4.get(1).value());
        assertEquals("acc ,var13,var14,var15,var16,var17,var18,var19,var20," + "var21,var22,var23 &",
                p4.get(2).value());
        assertEquals("acc ,var24,var25,var26,var27,var28,var29,var30)", p4.get(3).value());
    }
}
