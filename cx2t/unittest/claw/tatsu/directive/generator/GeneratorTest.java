/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.common.CompilerDirective;
import org.junit.Test;

import java.util.Collections;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.*;

/**
 * Test Directive generators classes
 *
 * @author clementval
 */
public class GeneratorTest {

  @Test
  public void directiveNoneTest() {
    DirectiveGenerator gen = new DirectiveNone();

    assertEquals("", gen.getPrefix());
    assertEquals("", gen.getPrefixCont());
    assertEquals(0, gen.getStartParallelDirective("").length);
    assertEquals(0, gen.getEndParallelDirective().length);
    assertEquals(0, gen.getStartLoopDirective(0, false, false, "").length);
    assertEquals(0, gen.getEndLoopDirective().length);
    assertEquals(0, gen.getSingleDirective("").length);
    assertEquals(0, gen.getRoutineDirective(false).length);
    assertEquals(0, gen.getRoutineDirective(true).length);
    assertEquals(0,
        gen.getStartDataRegion(Collections.<String>emptyList()).length);
    assertEquals(0, gen.getEndDataRegion().length);

    assertTrue(gen.getParallelKeyword().isEmpty());
    assertTrue(gen.getPrivateClause("").isEmpty());
    assertTrue(gen.getPrivateClause(Collections.<String>emptyList()).isEmpty());
    assertTrue(gen.getSequentialClause().isEmpty());

    assertFalse(gen.isCompileGuard(""));
    assertFalse(gen.isCompileGuard("acc loop"));

    assertSame(CompilerDirective.NONE, gen.getDirectiveLanguage());
  }

}
