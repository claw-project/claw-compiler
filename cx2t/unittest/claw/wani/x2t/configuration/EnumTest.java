/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import org.junit.Test;

import static junit.framework.TestCase.assertEquals;

/**
 * Testing methods from various enum class in configuration package.
 *
 * @author clementval
 */
public class EnumTest {

  @Test
  public void acceleratorDataStrategyCtorTest() {
    assertEquals(AcceleratorDataStrategy.NONE,
        AcceleratorDataStrategy.fromString(null));
    assertEquals(AcceleratorDataStrategy.NONE,
        AcceleratorDataStrategy.fromString(""));
    assertEquals(AcceleratorDataStrategy.NONE,
        AcceleratorDataStrategy.fromString("none"));
    assertEquals(AcceleratorDataStrategy.NONE,
        AcceleratorDataStrategy.fromString("NONE"));
    assertEquals(AcceleratorDataStrategy.PRESENT,
        AcceleratorDataStrategy.fromString("present"));
    assertEquals(AcceleratorDataStrategy.PRESENT,
        AcceleratorDataStrategy.fromString("PRESENT"));
    assertEquals(AcceleratorDataStrategy.KERNEL,
        AcceleratorDataStrategy.fromString("kernel"));
    assertEquals(AcceleratorDataStrategy.KERNEL,
        AcceleratorDataStrategy.fromString("KERNEL"));
  }
}
