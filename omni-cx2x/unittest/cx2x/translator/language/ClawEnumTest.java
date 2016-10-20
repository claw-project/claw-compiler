/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language;

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

}
