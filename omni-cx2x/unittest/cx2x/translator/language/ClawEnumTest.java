/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.target.Target;
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
    assertEquals(Target.CPU, Target.fromString("cpu"));
    assertEquals(Target.GPU, Target.fromString("gpu"));
    assertEquals(Target.MIC, Target.fromString("mic"));
    assertEquals(Target.CPU, Target.fromString("unknown"));
    assertEquals(Target.CPU, Target.fromString(null));
    assertEquals(Target.CPU, Target.fromString(""));
    assertEquals(Arrays.asList("cpu", "gpu", "mic"), Target.availableTargets());
  }

  @Test
  public void AcceleratorDirectiveTest() {
    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString(ClawConstant.DIRECTIVE_NONE));
    assertEquals(AcceleratorDirective.OPENACC,
        AcceleratorDirective.fromString(ClawConstant.DIRECTIVE_OPENACC));
    assertEquals(AcceleratorDirective.OPENMP,
        AcceleratorDirective.fromString(ClawConstant.DIRECTIVE_OPENMP));

    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString("unknown"));
    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString(null));
    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString(""));
    assertEquals(Arrays.asList(ClawConstant.DIRECTIVE_NONE,
        ClawConstant.DIRECTIVE_OPENACC, ClawConstant.DIRECTIVE_OPENMP),
        AcceleratorDirective.availableDirectiveLanguage());

    assertEquals(ClawConstant.OPENACC_PREFIX,
        AcceleratorDirective.getPrefix(AcceleratorDirective.OPENACC));
    assertEquals(ClawConstant.OPENMP_PREFIX,
        AcceleratorDirective.getPrefix(AcceleratorDirective.OPENMP));
    assertEquals(null,
        AcceleratorDirective.getPrefix(AcceleratorDirective.NONE));
    assertEquals(null, AcceleratorDirective.getPrefix(null));
  }

}
