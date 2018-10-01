/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.XmlHelper;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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

    Xnode p1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p1.setValue(CompilerDirective.OPENACC.getPrefix());
    assertEquals(CompilerDirective.OPENACC.getPrefix(), Pragma.getPrefix(p1));

    Xnode p2 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p2.setValue(CompilerDirective.OPENMP.getPrefix());
    assertEquals(CompilerDirective.OPENMP.getPrefix(), Pragma.getPrefix(p2));

    Xnode p3 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p3.setValue("");
    assertEquals("", Pragma.getPrefix(p3));

    Xnode p4 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    assertEquals("", Pragma.getPrefix(p4));

    Xnode p5 = xcodeml.createNode(Xcode.F_DO_STATEMENT);
    p5.setValue("acc");
    assertEquals("", Pragma.getPrefix(p5));

    assertEquals("", Pragma.getPrefix(null));

    Xnode p6 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p6.setValue(CompilerDirective.OPENACC.getPrefix() + " loop private(a)");
    assertEquals(CompilerDirective.OPENACC.getPrefix(), Pragma.getPrefix(p6));

    p6.setValue(CompilerDirective.OPENMP.getPrefix() + " target");
    assertEquals(CompilerDirective.OPENMP.getPrefix(), Pragma.getPrefix(p6));
  }

  @Test
  public void splitTest() {
    Context.get().init(CompilerDirective.OPENACC, Target.GPU, null, 80);
    String p1 = "acc data present(var1,var2,var3,var4,var5,var6,var7,var8," +
        "var10,var11,var12,var13,var14,var15,var16)";
    int maxCol = 40;

    // Just commas
    List<String> splitted =
        Pragma.split(p1, maxCol, CompilerDirective.OPENACC.getPrefix());
    checkSplitted(splitted, 5, maxCol);

    // Just spaces
    p1 = "acc data present(var1, var2, var3, var4, var5, var6, var7, " +
        "var8, var10, var11, var12, var13, var14, var15, var16)";
    splitted = Pragma.split(p1, maxCol, CompilerDirective.OPENACC.getPrefix());
    checkSplitted(splitted, 4, maxCol);

    // Mixed spaces and commas
    p1 = "acc data present(var1, var2,var3,var4, var5, var6, var7, " +
        "var8,var10,var11, var12,var13,var14, var15,var16, var17, var18)";
    splitted = Pragma.split(p1, maxCol, CompilerDirective.OPENACC.getPrefix());
    checkSplitted(splitted, 5, maxCol);

    p1 = "omp target teams distribute parallel do simd";
    splitted = Pragma.split(p1, maxCol, CompilerDirective.OPENMP.getPrefix());
    checkSplitted(splitted, 2, maxCol);
  }

  // Checks for the splitByLengthTest
  private void checkSplitted(List<String> splitted, int nb, int maxColumns) {
    Assert.assertEquals(nb, splitted.size());
    for(String chunk : splitted) {
      assertTrue(chunk.length() <= maxColumns);
    }
  }

  @Test
  public void pragmaCommentTest() {
    // OpenACC
    String p1 = "acc parallel";
    assertEquals(p1, Pragma.dropEndingComment(p1));
    String p2 = "acc parallel ! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p2));
    String p3 = "acc parallel !!! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p3));
    assertNull(Pragma.dropEndingComment(null));
    assertEquals("", Pragma.dropEndingComment(""));

    // OpenMP
    p1 = "omp target teams distribute";
    assertEquals(p1, Pragma.dropEndingComment(p1));
    p2 = "omp target teams distribute ! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p2));
    p3 = "omp target teams distribute !!! Start parallel region";
    assertEquals(p1, Pragma.dropEndingComment(p3));
  }

  @Test
  public void splitByContTest() {
    Context.get().init(CompilerDirective.OPENACC, Target.GPU, null, 80);

    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    List<FfunctionDefinition> fctDefs = xcodeml.getAllFctDef();
    assertTrue(!fctDefs.isEmpty());
    FfunctionDefinition fd = fctDefs.get(0);
    assertNotNull(fd.body());
    List<Xnode> previous = fd.matchAll(Xcode.F_PRAGMA_STATEMENT);
    Xnode p1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    fd.body().append(p1);
    p1.setValue("acc data present(q,acc& p,acc& h)acc& create(pt)");
    try {
      Pragma.splitByCont(p1, CompilerDirective.OPENACC.getPrefix(), xcodeml);
      List<Xnode> splittedPragma = fd.matchAll(Xcode.F_PRAGMA_STATEMENT);
      assertEquals(previous.size() + 4, splittedPragma.size());
    } catch(IllegalTransformationException e) {
      fail();
    }

    Xnode p2 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    fd.body().append(p2);
    p2.setValue("omp target teams omp& distribute simd");
    try {
      Pragma.splitByCont(p2, CompilerDirective.OPENMP.getPrefix(), xcodeml);
      List<Xnode> splittedPragma = fd.matchAll(Xcode.F_PRAGMA_STATEMENT);
      assertEquals(previous.size() + 6, splittedPragma.size());
    } catch(IllegalTransformationException e) {
      fail();
    }
  }

  @Test
  public void splitByContTest2() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    List<FfunctionDefinition> fctDefs = xcodeml.getAllFctDef();
    assertTrue(!fctDefs.isEmpty());
    FfunctionDefinition fd = fctDefs.get(0);
    assertNotNull(fd.body());
    List<Xnode> previous = fd.matchAll(Xcode.F_PRAGMA_STATEMENT);
    Xnode p = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    fd.body().append(p);
    p.setValue("acc data copyin(a) acc      present(b)");
    try {
      Pragma.splitByCont(p, CompilerDirective.OPENACC.getPrefix(), xcodeml);
      List<Xnode> splittedPragma = fd.matchAll(Xcode.F_PRAGMA_STATEMENT);
      assertEquals(previous.size() + 2, splittedPragma.size());
    } catch(IllegalTransformationException e) {
      fail();
    }
  }

  @Test
  public void getCompilerDirectiveTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode p1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    Xnode n1 = xcodeml.createNode(Xcode.F_DO_STATEMENT);

    p1.setValue("claw loop-fusion");
    assertEquals(CompilerDirective.CLAW, Pragma.getCompilerDirective(p1));

    p1.setValue("omp teams distribute");
    assertEquals(CompilerDirective.OPENMP, Pragma.getCompilerDirective(p1));

    p1.setValue("acc parallel");
    assertEquals(CompilerDirective.OPENACC, Pragma.getCompilerDirective(p1));

    p1.setValue("ivdep");
    assertEquals(CompilerDirective.NONE, Pragma.getCompilerDirective(p1));
    assertEquals(CompilerDirective.NONE, Pragma.getCompilerDirective(null));
    assertEquals(CompilerDirective.NONE, Pragma.getCompilerDirective(n1));
  }

}
