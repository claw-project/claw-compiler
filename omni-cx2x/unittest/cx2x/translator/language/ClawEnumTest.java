/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.common.OverPosition;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.xnode.ClawAttr;
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
    assertEquals(Target.CPU, Target.fromString("unknown"));
    assertEquals(Target.CPU, Target.fromString(null));
    assertEquals(Target.CPU, Target.fromString(""));
    assertEquals(Arrays.asList(ClawConstant.TARGET_CPU, ClawConstant.TARGET_GPU,
        ClawConstant.TARGET_MIC), Target.availableTargets());
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

  @Test
  public void OverPositionTest() {
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromString(ClawConstant.BEFORE));
    assertEquals(OverPosition.MIDDLE,
        OverPosition.fromString(ClawConstant.MIDDLE));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromString(ClawConstant.AFTER));
    assertEquals(OverPosition.BEFORE, OverPosition.fromString(null));
    assertEquals(OverPosition.BEFORE, OverPosition.fromString(""));
    assertEquals(OverPosition.BEFORE, OverPosition.fromString("unknown"));
  }


  @Test
  public void ClawAttrTest() {
    assertEquals(ClawAttr.IS_CLAW, ClawAttr.fromString(ClawConstant.IS_CLAW));
    assertEquals(ClawAttr.OVER, ClawAttr.fromString(ClawConstant.OVER));
    assertEquals(null, ClawAttr.fromString(null));
    assertEquals(null, ClawAttr.fromString(""));
    assertEquals(null, ClawAttr.fromString("unknown"));
  }
}
