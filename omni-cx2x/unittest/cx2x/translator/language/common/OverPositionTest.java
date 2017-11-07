/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language.common;

import cx2x.translator.common.ClawConstant;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

/**
 * @author clementval
 */
public class OverPositionTest {

  @Test
  public void fromStringTest() {
    assertEquals(OverPosition.BEFORE, OverPosition.fromString(null));
    assertEquals(OverPosition.BEFORE, OverPosition.fromString(""));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromString(ClawConstant.BEFORE));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromString(ClawConstant.AFTER));
    assertEquals(OverPosition.MIDDLE,
        OverPosition.fromString(ClawConstant.MIDDLE));
  }

  @Test
  public void fromListTest() {
    assertEquals(OverPosition.BEFORE, OverPosition.fromList(null));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Collections.<String>emptyList()));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromList(Arrays.asList(":", "col")));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Arrays.asList("col", ":")));
    assertEquals(OverPosition.MIDDLE,
        OverPosition.fromList(Arrays.asList(":", "col", ":")));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Arrays.asList("nproma", "col", ":")));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromList(Arrays.asList(":", "col", "nproma")));
  }

  @Test
  public void toStringTest() {
    assertEquals(ClawConstant.BEFORE, OverPosition.BEFORE.toString());
    assertEquals(ClawConstant.MIDDLE, OverPosition.MIDDLE.toString());
    assertEquals(ClawConstant.AFTER, OverPosition.AFTER.toString());
  }

}
