/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language.common;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

/**
 * @author clementval
 */
public class OverPositionTest {

  @Test
  public void fromListTest() {
    assertEquals(OverPosition.BEFORE, OverPosition.fromList(null));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Collections.<String>emptyList()));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Arrays.asList(":", "col")));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromList(Arrays.asList("col", ":")));
    assertEquals(OverPosition.MIDDLE,
        OverPosition.fromList(Arrays.asList(":", "col", ":")));
    assertEquals(OverPosition.BEFORE,
        OverPosition.fromList(Arrays.asList("nproma", "col", ":")));
    assertEquals(OverPosition.AFTER,
        OverPosition.fromList(Arrays.asList(":", "col", "nproma")));
  }

}
