/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.helper.XnodeUtil;
import helper.TestConstant;
import helper.XmlHelper;
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

  @Test
  public void getPragmaPrefixTest() {
    XcodeProgram xp = XmlHelper.getDummyXcodeProgram();

    Xnode p1 = new Xnode(Xcode.FPRAGMASTATEMENT, xp);
    p1.setValue(ClawConstant.OPENACC_PREFIX);
    assertEquals(ClawConstant.OPENACC_PREFIX, XnodeUtil.getPragmaPrefix(p1));

    Xnode p2 = new Xnode(Xcode.FPRAGMASTATEMENT, xp);
    p2.setValue(ClawConstant.OPENMP_PREFIX);
    assertEquals(ClawConstant.OPENMP_PREFIX, XnodeUtil.getPragmaPrefix(p2));

    Xnode p3 = new Xnode(Xcode.FPRAGMASTATEMENT, xp);
    p3.setValue("");
    assertEquals("", XnodeUtil.getPragmaPrefix(p3));

    Xnode p4 = new Xnode(Xcode.FPRAGMASTATEMENT, xp);
    assertEquals("", XnodeUtil.getPragmaPrefix(p4));

    Xnode p5 = new Xnode(Xcode.FDOSTATEMENT, xp);
    p5.setValue("acc");
    assertEquals("", XnodeUtil.getPragmaPrefix(p5));

    assertEquals("", XnodeUtil.getPragmaPrefix(null));

    Xnode p6 = new Xnode(Xcode.FPRAGMASTATEMENT, xp);
    p6.setValue(ClawConstant.OPENACC_PREFIX + " loop private(a)");
    assertEquals(ClawConstant.OPENACC_PREFIX, XnodeUtil.getPragmaPrefix(p6));
  }

  @Test
  public void splitByLengthTest() {
    String p1 = "acc data present(var1,var2,var3,var4,var5,var6,var7,var8," +
        "var10,var11,var12,var13,var14,var15,var16)";
    int maxCol = 40;

    // Just commas
    List<String> splitted =
        XnodeUtil.splitByLength(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 5, maxCol);

    // Just spaces
    p1 = "acc data present(var1, var2, var3, var4, var5, var6, var7, " +
        "var8, var10, var11, var12, var13, var14, var15, var16)";
    splitted = XnodeUtil.splitByLength(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 4, maxCol);

    // Mixed spaces and commas
    p1 = "acc data present(var1, var2,var3,var4, var5, var6, var7, " +
        "var8,var10,var11, var12,var13,var14, var15,var16, var17, var18)";
    splitted = XnodeUtil.splitByLength(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 5, maxCol);
  }

  // Checks for the splitByLengthTest
  private void checkSplitted(List<String> splitted, int nb, int maxColumns) {
    assertEquals(nb, splitted.size());
    for(String chunk : splitted) {
      assertTrue(chunk.length() <= maxColumns);
    }
  }
}
