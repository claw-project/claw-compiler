/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.transformation.primitive.Pragma;
import cx2x.xcodeml.helper.HoistedNestedDoStatement;
import cx2x.xcodeml.helper.XnodeUtil;
import helper.TestConstant;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test methods of the XnodeUtil class
 *
 * @author clementval
 */
public class XnodeUtilTest {

  @Test
  public void xpathIntersectTest() {
    File f = new File(TestConstant.TEST_PROGRAM);
    assertTrue(f.exists());
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_PROGRAM);
    assertNotNull(xcodeml);

    List<Xnode> pragmas = xcodeml.getAllStmt(Xcode.FPRAGMASTATEMENT);
    assertEquals(4, pragmas.size());

    Xnode loopHoistStart = new Xnode(pragmas.get(1).element());
    assertNotNull(loopHoistStart);
    assertTrue(loopHoistStart.value().contains("loop-hoist"));
    Xnode loopHoistEnd = new Xnode(pragmas.get(2).element());
    assertNotNull(loopHoistEnd);
    assertTrue(loopHoistEnd.value().contains("end loop-hoist"));

    List<HoistedNestedDoStatement> stmts =
        XnodeUtil.findDoStatementForHoisting(loopHoistStart, loopHoistEnd,
            Arrays.asList("j", "i"));

    assertEquals(3, stmts.size());
  }

  @Test
  public void gatherArgumentsTest() {
    String arg1 = "nz";
    String arg2 = "q(:,1:60)";
    String arg3 = "ty%y(:,:)";
    String arg4 = "z(:)";
    String arg5 = "nproma";

    File f = new File(TestConstant.TEST_ARGUMENTS);
    assertTrue(f.exists());
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_ARGUMENTS);
    assertNotNull(xcodeml);

    List<Xnode> functionCalls = xcodeml.matchAll(Xcode.FUNCTIONCALL);
    assertEquals(1, functionCalls.size());

    Xnode fctCall = functionCalls.get(0);
    assertTrue(fctCall.opcode() == Xcode.FUNCTIONCALL);

    List<String> allArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.ANY, false);
    assertEquals(5, allArguments.size());
    assertEquals(arg1, allArguments.get(0));
    assertEquals(arg2, allArguments.get(1));
    assertEquals(arg3, allArguments.get(2));
    assertEquals(arg4, allArguments.get(3));
    assertEquals(arg5, allArguments.get(4));

    List<String> inArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.IN, false);
    assertEquals(5, inArguments.size());
    assertEquals(arg1, inArguments.get(0));
    assertEquals(arg2, inArguments.get(1));
    assertEquals(arg3, inArguments.get(2));
    assertEquals(arg4, inArguments.get(3));
    assertEquals(arg5, inArguments.get(4));

    List<String> outArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.OUT, false);
    assertEquals(3, outArguments.size());
    assertEquals(arg2, outArguments.get(0));
    assertEquals(arg3, outArguments.get(1));
    assertEquals(arg4, outArguments.get(2));

    List<String> inArrayArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.IN, true);
    assertEquals(3, inArrayArguments.size());
    assertEquals(arg2, inArrayArguments.get(0));
    assertEquals(arg3, inArrayArguments.get(1));
    assertEquals(arg4, inArrayArguments.get(2));

    List<String> outArrayArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.OUT, true);
    assertEquals(3, outArrayArguments.size());
    assertEquals(arg2, outArrayArguments.get(0));
    assertEquals(arg3, outArrayArguments.get(1));
    assertEquals(arg4, outArrayArguments.get(2));

    List<String> inOutArrayArguments =
        XnodeUtil.gatherArguments(xcodeml, fctCall, Xintent.INOUT, true);
    assertEquals(3, inOutArrayArguments.size());
    assertEquals(arg2, inOutArrayArguments.get(0));
    assertEquals(arg3, inOutArrayArguments.get(1));
    assertEquals(arg4, inOutArrayArguments.get(2));

    // TODO add test with optional arguments
  }
}
