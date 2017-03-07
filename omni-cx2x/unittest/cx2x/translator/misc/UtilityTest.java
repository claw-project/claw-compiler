/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.misc;


import cx2x.translator.common.Utility;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;


/**
 * Join utility test
 *
 * @author clementval
 */
public class UtilityTest {

  @Test
  public void JoinArrayTest() {
    String[] a = {"a", "b", "c"};
    assertEquals("a,b,c", Utility.join(",", a));
    String[] b = {"a"};
    assertEquals("a", Utility.join(",", b));
  }

  @Test
  public void JoinListTest() {
    List<String> a = new ArrayList<>();
    a.add("a");
    a.add("b");
    a.add("c");
    assertEquals("a,b,c", Utility.join(",", a));
    List<String> b = new ArrayList<>();
    b.add("a");
    assertEquals("a", Utility.join(",", b));
  }
}
