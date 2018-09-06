/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.TatsuConstant;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test enum OpenAccExecutionMode
 *
 * @author clementval
 */
public class OpenAccExecutionModeTest {

  @Test
  public void executionModeTest() {
    // Test fromString method
    assertEquals(OpenAccExecutionMode.NONE,
        OpenAccExecutionMode.fromString(""));
    assertEquals(OpenAccExecutionMode.NONE,
        OpenAccExecutionMode.fromString(null));
    assertEquals(OpenAccExecutionMode.NONE,
        OpenAccExecutionMode.fromString("dummy"));
    assertEquals(OpenAccExecutionMode.VECTOR,
        OpenAccExecutionMode.
            fromString(TatsuConstant.OPENACC_EXEC_MODE_VECTOR));
    assertEquals(OpenAccExecutionMode.GANG,
        OpenAccExecutionMode.
            fromString(TatsuConstant.OPENACC_EXEC_MODE_GANG));
    assertEquals(OpenAccExecutionMode.GANG_VECTOR,
        OpenAccExecutionMode.fromString(
            TatsuConstant.OPENACC_EXEC_MODE_GANG_VECTOR));

    // Test getFormattedExecutionMode method
    assertEquals("",
        OpenAccExecutionMode.NONE.getFormattedExecutionMode());
    assertEquals("vector",
        OpenAccExecutionMode.VECTOR.getFormattedExecutionMode());
    assertEquals("gang", OpenAccExecutionMode.GANG.getFormattedExecutionMode());
    assertEquals("gang vector", OpenAccExecutionMode.GANG_VECTOR.
        getFormattedExecutionMode());
  }

}
