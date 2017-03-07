/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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

    List<Xnode> stmts =
        XnodeUtil.findDoStatement(loopHoistStart, loopHoistEnd,
            Arrays.asList("j", "i"));

    assertEquals(3, stmts.size());


  }
}
