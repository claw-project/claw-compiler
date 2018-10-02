/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import org.junit.Test;

import java.util.*;

import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

/**
 * Join utility test
 *
 * @author clementval
 */
public class UtilityTest {

  @Test
  public void joinArrayTest() {
    String[] a = {"a", "b", "c"};
    assertEquals("a,b,c", Utility.join(",", a));
    String[] b = {"a"};
    assertEquals("a", Utility.join(",", b));
  }

  @Test
  public void joinListTest() {
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

  private Object getRawList() {
    return new ArrayList<>(Arrays.asList("a", "b", null, "c"));
  }

  @Test
  public void convertToListTest() {
    List<String> recovered = Utility.convertToList(getRawList());
    assertNotNull(recovered);
    assertEquals(4, recovered.size());
    assertEquals("a", recovered.get(0));
    assertEquals("b", recovered.get(1));
    assertNull(recovered.get(2));
    assertEquals("c", recovered.get(3));
  }

  private Object getRawMap() {
    Map<String, String> map = new HashMap<>();
    map.put("a", "layout1");
    map.put("b", null);
    map.put("c", null);
    return map;
  }

  @Test
  public void convertToMapTest() {
    Map<String, String> recovered = Utility.convertToMap(getRawMap());
    assertNotNull(recovered);

    assertTrue(recovered.containsKey("a"));
    assertTrue(recovered.containsKey("b"));
    assertTrue(recovered.containsKey("c"));

    assertEquals("layout1", recovered.get("a"));
    assertNull(recovered.get("b"));
    assertNull(recovered.get("c"));
  }

}
