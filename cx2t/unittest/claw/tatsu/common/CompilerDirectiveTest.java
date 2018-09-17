/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.TatsuConstant;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.directive.generator.OpenMp;
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
        CompilerDirective.fromString(OpenMp.OPENMP_NAME));
    assertEquals(CompilerDirective.OPENMP,
        CompilerDirective.fromString(OpenMp.OPENMP_PREFIX));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString(OpenAcc.OPENACC_NAME));
    assertEquals(CompilerDirective.OPENACC,
        CompilerDirective.fromString(OpenAcc.OPENACC_PREFIX));

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

    assertEquals(OpenAcc.OPENACC_NAME,
        CompilerDirective.OPENACC.toString());
    assertEquals(OpenMp.OPENMP_NAME, CompilerDirective.OPENMP.toString());
    assertEquals(TatsuConstant.DIRECTIVE_NONE,
        CompilerDirective.NONE.toString());

    assertEquals(OpenMp.OPENMP_PREFIX, CompilerDirective.OPENMP.getPrefix());
    assertEquals(OpenAcc.OPENACC_PREFIX, CompilerDirective.OPENACC.getPrefix());
  }
}
