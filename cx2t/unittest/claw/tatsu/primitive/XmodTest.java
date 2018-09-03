/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.wani.x2t.configuration.AcceleratorConfiguration;
import claw.wani.x2t.configuration.OpenAccConfiguration;
import org.junit.Test;

import java.util.HashMap;

import static junit.framework.TestCase.assertEquals;

/**
 * Test methods of the Xmod class.
 *
 * @author clementval
 */
public class XmodTest {

  @Test
  public void getSuffixTest() {
    AcceleratorConfiguration config = new OpenAccConfiguration(new HashMap<>());
    Context.init(CompilerDirective.OPENACC, Target.GPU, config, 80);
    // .[directive].[target].claw
    assertEquals(".openacc.gpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENMP, Target.CPU, config, 80);
    assertEquals(".openmp.cpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.CPU, config, 80);
    assertEquals(".none.cpu.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENMP, Target.MIC, config, 80);
    assertEquals(".openmp.mic.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.FPGA, config, 80);
    assertEquals(".none.fpga.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.OPENACC, null, config, 80);
    assertEquals(".openacc.none.claw.xmod", Xmod.getSuffix());
    Context.init(null, null, null, 80);
    assertEquals(".none.none.claw.xmod", Xmod.getSuffix());
    Context.init(CompilerDirective.NONE, Target.GPU, config, 80);
    assertEquals(".none.gpu.claw.xmod", Xmod.getSuffix());
  }
}
