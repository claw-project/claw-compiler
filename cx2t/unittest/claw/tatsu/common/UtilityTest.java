/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

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

  @Test
  public void countOccurrenceTest() {
    assertEquals(0, Utility.countOccurrences("", ""));
    assertEquals(0, Utility.countOccurrences(null, null));
    assertEquals(0, Utility.countOccurrences("acc private acc", ""));
    assertEquals(2, Utility.countOccurrences("acc private acc", "acc"));
    assertEquals(2, Utility.countOccurrences("acc private acc", "ACC"));
    assertEquals(0, Utility.countOccurrences("acc private acc", "omp"));
    assertEquals(0, Utility.countOccurrences("acc private acc", "OMP"));
  }
}
