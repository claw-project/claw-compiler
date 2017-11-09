/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import helper.XmlHelper;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertTrue;

/**
 * Test methods of the Pragma class.
 *
 * @author clementval
 */
public class PragmaTest {

  @Test
  public void getPragmaPrefixTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();

    Xnode p1 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    p1.setValue(ClawConstant.OPENACC_PREFIX);
    assertEquals(ClawConstant.OPENACC_PREFIX, Pragma.getPrefix(p1));

    Xnode p2 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    p2.setValue(ClawConstant.OPENMP_PREFIX);
    assertEquals(ClawConstant.OPENMP_PREFIX, Pragma.getPrefix(p2));

    Xnode p3 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    p3.setValue("");
    assertEquals("", Pragma.getPrefix(p3));

    Xnode p4 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    assertEquals("", Pragma.getPrefix(p4));

    Xnode p5 = xcodeml.createNode(Xcode.FDOSTATEMENT);
    p5.setValue("acc");
    assertEquals("", Pragma.getPrefix(p5));

    assertEquals("", Pragma.getPrefix(null));

    Xnode p6 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    p6.setValue(ClawConstant.OPENACC_PREFIX + " loop private(a)");
    assertEquals(ClawConstant.OPENACC_PREFIX, Pragma.getPrefix(p6));
  }

  @Test
  public void splitTest() {
    String p1 = "acc data present(var1,var2,var3,var4,var5,var6,var7,var8," +
        "var10,var11,var12,var13,var14,var15,var16)";
    int maxCol = 40;

    // Just commas
    List<String> splitted =
        Pragma.split(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 5, maxCol);

    // Just spaces
    p1 = "acc data present(var1, var2, var3, var4, var5, var6, var7, " +
        "var8, var10, var11, var12, var13, var14, var15, var16)";
    splitted = Pragma.split(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 4, maxCol);

    // Mixed spaces and commas
    p1 = "acc data present(var1, var2,var3,var4, var5, var6, var7, " +
        "var8,var10,var11, var12,var13,var14, var15,var16, var17, var18)";
    splitted = Pragma.split(p1, maxCol, ClawConstant.OPENACC_PREFIX);
    checkSplitted(splitted, 5, maxCol);
  }

  // Checks for the splitByLengthTest
  private void checkSplitted(List<String> splitted, int nb, int maxColumns) {
    Assert.assertEquals(nb, splitted.size());
    for(String chunk : splitted) {
      assertTrue(chunk.length() <= maxColumns);
    }
  }

  @Test
  public void PragmaCommentTest() {
    String p1 = "acc parallel";
    assertEquals(p1, Pragma.dropEndingComment(p1));
    String p2 = "acc parallel ! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p2));
    String p3 = "acc parallel !!! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p3));
    assertEquals(null, Pragma.dropEndingComment(null));
    assertEquals("", Pragma.dropEndingComment(""));
  }

}
