/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.abstraction.HoistedNestedDoStatement;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test methods of the XnodeUtil class
 *
 * @author clementval
 */
public class XnodeUtilTest
{

    @Test
    public void xpathIntersectTest()
    {
        Context context = new TestContext();
        assertTrue(Files.exists(TestConstant.TEST_PROGRAM));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_PROGRAM, context);
        assertNotNull(xcodeml);

        List<Xnode> pragmas = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
        assertEquals(4, pragmas.size());

        Xnode loopHoistStart = new Xnode(pragmas.get(1).element());
        assertNotNull(loopHoistStart);
        assertTrue(loopHoistStart.value().contains("loop-hoist"));
        Xnode loopHoistEnd = new Xnode(pragmas.get(2).element());
        assertNotNull(loopHoistEnd);
        assertTrue(loopHoistEnd.value().contains("end loop-hoist"));

        List<HoistedNestedDoStatement> stmts = XnodeUtil.findDoStatementForHoisting(loopHoistStart, loopHoistEnd,
                Arrays.asList("j", "i"));

        assertEquals(3, stmts.size());
    }

    @Test
    public void gatherArgumentsTest()
    {
        Context context = new TestContext();
        String arg1 = "nz";
        String arg2 = "q(:,1:60)";
        String arg3 = "ty%y(:,:)";
        String arg4 = "z(:)";
        String arg5 = "nproma";

        assertTrue(Files.exists(TestConstant.TEST_ARGUMENTS));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_ARGUMENTS, context);
        assertNotNull(xcodeml);

        List<Xnode> functionCalls = xcodeml.matchAll(Xcode.FUNCTION_CALL);
        assertEquals(1, functionCalls.size());

        FunctionCall fctCall = new FunctionCall(functionCalls.get(0));
        assertSame(fctCall.opcode(), Xcode.FUNCTION_CALL);

        FfunctionType fctType = xcodeml.getTypeTable().getFunctionType(fctCall);

        List<String> allArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.ANY, false, false);
        assertEquals(5, allArguments.size());
        assertEquals(arg1, allArguments.get(0));
        assertEquals(arg2, allArguments.get(1));
        assertEquals(arg3, allArguments.get(2));
        assertEquals(arg4, allArguments.get(3));
        assertEquals(arg5, allArguments.get(4));

        List<String> inArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.IN, false, false);
        assertEquals(5, inArguments.size());
        assertEquals(arg1, inArguments.get(0));
        assertEquals(arg2, inArguments.get(1));
        assertEquals(arg3, inArguments.get(2));
        assertEquals(arg4, inArguments.get(3));
        assertEquals(arg5, inArguments.get(4));

        List<String> outArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.OUT, false, false);
        assertEquals(3, outArguments.size());
        assertEquals(arg2, outArguments.get(0));
        assertEquals(arg3, outArguments.get(1));
        assertEquals(arg4, outArguments.get(2));

        List<String> inArrayArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.IN, true, false);
        assertEquals(3, inArrayArguments.size());
        assertEquals(arg2, inArrayArguments.get(0));
        assertEquals(arg3, inArrayArguments.get(1));
        assertEquals(arg4, inArrayArguments.get(2));

        List<String> outArrayArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.OUT, true, false);
        assertEquals(3, outArrayArguments.size());
        assertEquals(arg2, outArrayArguments.get(0));
        assertEquals(arg3, outArrayArguments.get(1));
        assertEquals(arg4, outArrayArguments.get(2));

        List<String> inOutArrayArguments = fctCall.gatherArguments(xcodeml, fctType, xcodeml, Intent.INOUT, true,
                false);
        assertEquals(3, inOutArrayArguments.size());
        assertEquals(arg2, inOutArrayArguments.get(0));
        assertEquals(arg3, inOutArrayArguments.get(1));
        assertEquals(arg4, inOutArrayArguments.get(2));

        // TODO add test with optional arguments
    }
}
