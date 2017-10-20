/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator;

import cx2x.translator.common.ClawConstant;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test method of the AcceleratorDirective class
 *
 * @author clementval
 */
public class AcceleratorDirectiveTest {

  @Test
  public void ctorTest() {
    assertEquals(AcceleratorDirective.OPENMP,
        AcceleratorDirective.fromString("openmp"));
    assertEquals(AcceleratorDirective.OPENACC,
        AcceleratorDirective.fromString("openacc"));

    assertEquals(AcceleratorDirective.OPENMP,
        AcceleratorDirective.fromString("OPENMP"));
    assertEquals(AcceleratorDirective.OPENACC,
        AcceleratorDirective.fromString("OPENACC"));

    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString(null));
    assertEquals(AcceleratorDirective.NONE,
        AcceleratorDirective.fromString(""));

    assertEquals(ClawConstant.DIRECTIVE_OPENACC,
        AcceleratorDirective.OPENACC.toString());
    assertEquals(ClawConstant.DIRECTIVE_OPENMP,
        AcceleratorDirective.OPENMP.toString());
    assertEquals(ClawConstant.DIRECTIVE_NONE,
        AcceleratorDirective.NONE.toString());

    assertEquals(ClawConstant.OPENMP_PREFIX,
        AcceleratorDirective.getPrefix(AcceleratorDirective.OPENMP));
    assertEquals(ClawConstant.OPENACC_PREFIX,
        AcceleratorDirective.getPrefix(AcceleratorDirective.OPENACC));
  }

}
