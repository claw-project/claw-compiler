/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator;

import cx2x.translator.common.ClawConstant;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test method of the CompilerDirective class
 *
 * @author clementval
 */
public class CompilerDirectiveTest {

  @Test
  public void ctorTest() {
    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString("openmp"));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString("openacc"));

    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString("OPENMP"));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString("OPENACC"));

    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(null));
    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(""));

    assertEquals(ClawConstant.DIRECTIVE_OPENACC,
        CompilerDirective.OPENACC.toString());
    assertEquals(ClawConstant.DIRECTIVE_OPENMP,
        CompilerDirective.OPENMP.toString());
    assertEquals(ClawConstant.DIRECTIVE_NONE,
        CompilerDirective.NONE.toString());

    assertEquals(ClawConstant.OPENMP_PREFIX,
        CompilerDirective.getPrefix(CompilerDirective.OPENMP));
    assertEquals(ClawConstant.OPENACC_PREFIX,
        CompilerDirective.getPrefix(CompilerDirective.OPENACC));
  }

}
