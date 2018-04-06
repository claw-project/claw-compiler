/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.TatsuConstant;
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
        CompilerDirective.fromString(TatsuConstant.DIRECTIVE_OPENMP));
    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString(TatsuConstant.DIRECTIVE_SHORT_OPENMP));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString(TatsuConstant.DIRECTIVE_OPENACC));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString(TatsuConstant.DIRECTIVE_SHORT_OPENACC));

    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString("OPENMP"));
    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString("OMP"));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString("OPENACC"));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString("ACC"));

    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(null));
    assertEquals(CompilerDirective.NONE,
        CompilerDirective.fromString(""));

    assertEquals(TatsuConstant.DIRECTIVE_OPENACC,
        CompilerDirective.OPENACC.toString());
    assertEquals(TatsuConstant.DIRECTIVE_OPENMP,
        CompilerDirective.OPENMP.toString());
    assertEquals(TatsuConstant.DIRECTIVE_NONE,
        CompilerDirective.NONE.toString());

    assertEquals(TatsuConstant.OPENMP_PREFIX,
        CompilerDirective.OPENMP.getPrefix());
    assertEquals(TatsuConstant.OPENACC_PREFIX,
        CompilerDirective.OPENACC.getPrefix());
  }
}
