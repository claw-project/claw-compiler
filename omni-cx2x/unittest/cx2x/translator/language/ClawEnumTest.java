/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language;

import cx2x.translator.common.ClawConstant;
import cx2x.configuration.CompilerDirective;
import cx2x.configuration.Target;
import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertEquals;

/**
 * Test the features of various enumeration in the CLAW translator.
 *
 * @author clementval
 */
public class ClawEnumTest {

  @Test
  public void TargetTest() {
    assertEquals(Target.CPU, Target.fromString(ClawConstant.TARGET_CPU));
    assertEquals(Target.GPU, Target.fromString(ClawConstant.TARGET_GPU));
    assertEquals(Target.MIC, Target.fromString(ClawConstant.TARGET_MIC));
    assertEquals(Target.NONE, Target.fromString("unknown"));
    assertEquals(Target.NONE, Target.fromString(null));
    assertEquals(Target.NONE, Target.fromString(""));
    assertEquals(Arrays.asList(ClawConstant.TARGET_CPU, ClawConstant.TARGET_GPU,
        ClawConstant.TARGET_MIC, ClawConstant.TARGET_FPGA,
        ClawConstant.TARGET_NONE),
        Target.availableTargets());
  }

  @Test
  public void AcceleratorDirectiveTest() {
    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(ClawConstant.DIRECTIVE_NONE));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString(ClawConstant.DIRECTIVE_OPENACC));
    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString(ClawConstant.DIRECTIVE_OPENMP));

    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString("unknown"));
    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(null));
    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(""));
    assertEquals(Arrays.asList(ClawConstant.DIRECTIVE_NONE,
        ClawConstant.DIRECTIVE_OPENACC, ClawConstant.DIRECTIVE_OPENMP),
        CompilerDirective.availableDirectiveLanguage());

    assertEquals(ClawConstant.OPENACC_PREFIX,
        CompilerDirective.getPrefix(CompilerDirective.OPENACC));
    assertEquals(ClawConstant.OPENMP_PREFIX,
        CompilerDirective.getPrefix(CompilerDirective.OPENMP));
    assertEquals(null,
        CompilerDirective.getPrefix(CompilerDirective.NONE));
    assertEquals(null, CompilerDirective.getPrefix(null));
  }
}
