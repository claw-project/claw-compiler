/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 *
 */
package claw.tatsu.directive.generator;

import claw.tatsu.TatsuConstant;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test enum OpenMpExecutionMode
 *
 * @author clementval
 */
public class OpenMpExecutionModeTest {

  @Test
  public void executionModeTest() {
    // Test fromString method
    assertEquals(OpenMpExecutionMode.NONE, OpenMpExecutionMode.fromString(""));
    assertEquals(OpenMpExecutionMode.NONE,
        OpenMpExecutionMode.fromString(null));
    assertEquals(OpenMpExecutionMode.NONE,
        OpenMpExecutionMode.fromString("dummy"));
    assertEquals(OpenMpExecutionMode.TEAMS_DISTRIBUTE,
        OpenMpExecutionMode.
            fromString(TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE));
    assertEquals(OpenMpExecutionMode.TEAMS_DISTRIBUTE_SIMD,
        OpenMpExecutionMode.
            fromString(TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_SIMD));
    assertEquals(OpenMpExecutionMode.TEAMS_DISTRIBUTE_PARALLEL_DO,
        OpenMpExecutionMode.fromString(
            TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_PARALLEL_DO));
    assertEquals(OpenMpExecutionMode.TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD,
        OpenMpExecutionMode.fromString(
            TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD));

    // Test getFormattedExecutionMode method
    assertEquals("",
        OpenMpExecutionMode.NONE.getFormattedExecutionMode());
    assertEquals("teams distribute",
        OpenMpExecutionMode.TEAMS_DISTRIBUTE.getFormattedExecutionMode());
    assertEquals("teams distribute simd",
        OpenMpExecutionMode.TEAMS_DISTRIBUTE_SIMD.getFormattedExecutionMode());
    assertEquals("teams distribute parallel do",
        OpenMpExecutionMode.TEAMS_DISTRIBUTE_PARALLEL_DO.
            getFormattedExecutionMode());
    assertEquals("teams distribute parallel do simd",
        OpenMpExecutionMode.TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD.
            getFormattedExecutionMode());
  }
}
