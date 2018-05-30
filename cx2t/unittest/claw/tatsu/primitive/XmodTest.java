/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

/**
 * Test methods of the Xmod class.
 *
 * @author clementval
 */
public class XmodTest {

  @Test
  public void getSuffixTest() {
    Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
    // .[directive].[target].claw
    assertEquals(".openacc.gpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENMP, Target.CPU, 80);
    assertEquals(".openmp.cpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.CPU, 80);
    assertEquals(".none.cpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENMP, Target.MIC, 80);
    assertEquals(".openmp.mic.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.FPGA, 80);
    assertEquals(".none.fpga.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENACC, null, 80);
    assertEquals(".openacc.none.claw.xmod", Xmod.getSuffix());
    Context.init(null, null, 80);
    assertEquals(".none.none.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.GPU, 80);
    assertEquals(".none.gpu.claw.xmod", Xmod.getSuffix());
  }
}
