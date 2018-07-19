/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language.language;

import claw.tatsu.TatsuConstant;
import claw.tatsu.common.Target;
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
  public void targetTest() {
    assertEquals(Target.CPU, Target.fromString(TatsuConstant.TARGET_CPU));
    assertEquals(Target.GPU, Target.fromString(TatsuConstant.TARGET_GPU));
    assertEquals(Target.MIC, Target.fromString(TatsuConstant.TARGET_MIC));
    assertEquals(Target.NONE, Target.fromString("unknown"));
    assertEquals(Target.NONE, Target.fromString(null));
    assertEquals(Target.NONE, Target.fromString(""));
    assertEquals(Arrays.asList(TatsuConstant.TARGET_ARM,
        TatsuConstant.TARGET_CPU, TatsuConstant.TARGET_GPU,
        TatsuConstant.TARGET_MIC, TatsuConstant.TARGET_FPGA,
        TatsuConstant.TARGET_NONE),
        Target.availableTargets());
  }
}
