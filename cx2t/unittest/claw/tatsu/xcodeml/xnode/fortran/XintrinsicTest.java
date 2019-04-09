/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import org.junit.Test;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertNull;

/**
 * Test Xintrinsic class
 *
 * @author clementval
 */
public class XintrinsicTest {

  @Test
  public void xIntrinsicTest() {
    assertNull(Xintrinsic.fromString(null));
    assertNull(Xintrinsic.fromString("dummy"));
    assertNotNull(Xintrinsic.fromString("allocated"));
    assertEquals(Xintrinsic.ALLOCATED, Xintrinsic.fromString("allocated"));
  }
}
