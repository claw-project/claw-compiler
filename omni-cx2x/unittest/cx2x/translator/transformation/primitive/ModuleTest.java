/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.translator.config.Configuration;
import cx2x.translator.language.accelerator.AcceleratorDirective;
import cx2x.translator.language.base.Target;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

/**
 * Test methods of the Module class.
 *
 * @author clementval
 */
public class ModuleTest {

  @Test
  public void getSuffixTest() {
    Configuration config = Configuration.get();
    config.init(AcceleratorDirective.OPENACC, Target.GPU);
    // .[directive].[target].claw
    assertEquals(".openacc.gpu.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.OPENMP, Target.CPU);
    assertEquals(".openmp.cpu.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.NONE, Target.CPU);
    assertEquals(".none.cpu.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.OPENMP, Target.MIC);
    assertEquals(".openmp.mic.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.NONE, Target.FPGA);
    assertEquals(".none.fpga.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.OPENACC, null);
    assertEquals(".openacc.none.claw.xmod", Module.getSuffix());
    config.init(null, null);
    assertEquals(".none.none.claw.xmod", Module.getSuffix());
    config.init(AcceleratorDirective.NONE, Target.GPU);
    assertEquals(".none.gpu.claw.xmod", Module.getSuffix());
  }
}
