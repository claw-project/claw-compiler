/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import static org.junit.Assert.*;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import org.junit.Test;

import java.util.Arrays;
import java.util.List;

/**
 * JUnit test class for the CLAW language parser and information holding class.
 *
 * @author clementval
 */
public class ClawLanguageTest {

  /**
   * Test various input for the CLAW loop fusion directive.
   */
  @Test
  public void FusionTest(){
    // Valid directives
    analyzeValidClawLoopFusion("claw loop-fusion", null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1)", "g1");
    analyzeValidClawLoopFusion("claw loop-fusion group( g1 )", "g1");
    analyzeValidClawLoopFusion("claw loop-fusion group ( g1   ) ", "g1");
    analyzeValidClawLoopFusion("claw loop-fusiongroup(g1)", "g1");

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-fusion group");
    analyzeUnvalidClawLanguage("claw loop-fusion (i,j,k)");
    analyzeUnvalidClawLanguage("claw loop-fusion group()");
    analyzeUnvalidClawLanguage("claw loop-fusion group(   )");
  }

  /**
   * Assert the result for valid loop fusion CLAW directive
   * @param raw       Raw string valud of the CLAW directive to be analyzed.
   * @param groupName Group name to be found if any.
   */
  private void analyzeValidClawLoopFusion(String raw, String groupName){
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(ClawDirective.LOOP_FUSION, l.getDirective());
      if(groupName != null){
        assertTrue(l.hasGroupOption());
        assertEquals(groupName, l.getGroupName());
      } else {
        assertFalse(l.hasGroupOption());
        assertNull(l.getGroupName());
      }
    } catch(IllegalDirectiveException idex){
      fail();
    }
  }

  /**
   * Assert any unvalid claw raw input
   * @param raw Raw string valud of the CLAW directive to be analyzed.
   */
  private void analyzeUnvalidClawLanguage(String raw){
    ClawLanguage l = null;
    try {
      l = ClawLanguage.analyze(raw);
      fail();
    } catch (IllegalDirectiveException pex){
      assertNull(l);
      assertNotNull(pex);
      assertNotNull(pex.getMessage());
    }
  }

  /**
   * Test various input for the CLAW loop interchange directive.
   */
  @Test
  public void InterchangeTest(){
    // Valid directives
    analyzeValidClawLoopInterchange("claw loop-interchange", null);
    analyzeValidClawLoopInterchange("claw loop-interchange (i,j,k)",
        Arrays.asList("i", "j", "k"));
    analyzeValidClawLoopInterchange("claw loop-interchange (  i,j,k  ) ",
        Arrays.asList("i", "j", "k"));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-interchange ()");
    analyzeUnvalidClawLanguage("claw loop-interchange (i,j,k) group");
  }

  /**
   * Assert the result for valid loop interchange CLAW directive
   * @param raw       Raw string valud of the CLAW directive to be analyzed.
   * @param indexes   List of indexes to be found if any.
   */
  private void analyzeValidClawLoopInterchange(String raw,
                                               List<String> indexes)
  {
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(ClawDirective.LOOP_INTERCHANGE, l.getDirective());
      if(indexes != null){
        assertTrue(l.hasIndexes());
        assertEquals(indexes.size(), l.getIndexes().size());
      } else {
        assertFalse(l.hasIndexes());
        assertNull(l.getIndexes());
      }
    } catch(IllegalDirectiveException idex){
      fail();
    }
  }

  /**
   * Test various input for the CLAW remove directive.
   */
  @Test
  public void RemoveTest(){
    // Valid directives
    analyzeValidSimpleClaw("claw remove", ClawDirective.REMOVE);
    analyzeValidSimpleClaw("claw end remove", ClawDirective.END_REMOVE);
    analyzeValidSimpleClaw("claw   end   remove  ", ClawDirective.END_REMOVE);

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw");
    analyzeUnvalidClawLanguage("claw dummy");
    analyzeUnvalidClawLanguage("claw end re move");
  }

  /**
   * Assert the result for valid lo CLAW directive
   * @param raw       Raw string valud of the CLAW directive to be analyzed.
   * @param directive Directive to be match.
   */
  private void analyzeValidSimpleClaw(String raw, ClawDirective directive) {
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(directive, l.getDirective());
    } catch(IllegalDirectiveException idex){
      fail();
    }
  }


  /**
   * Test various input for the CLAW loop extract directive.
   */
  @Test
  public void ExtractTest(){
    // Valid directives
    ClawLanguage l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j)", "i", "istart",
        "iend", null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    ClawMapping map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend,2) map(i:j)", "i", "istart",
        "iend", "2");
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    l = analyzeValidClawLoopExtract("claw loop-extract range(i=1,10) map(i:j)",
        "i", "1", "10", null);
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=1,10,2) map(i:j) parallel",  "i", "1", "10", "2");
    map = l.getMappings().get(0);
    assertTrue(l.hasParallelOption());
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion", "i", "istart",
        "iend", null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionOption());
    assertFalse(l.hasGroupOption());
    assertFalse(l.hasParallelOption());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());


    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion group(j1)",
        "i", "istart", "iend", null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionOption());
    assertTrue(l.hasGroupOption());
    assertEquals("j1", l.getGroupName());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion group(j1) " +
            "acc(loop gang vector)", "i", "istart", "iend", null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionOption());
    assertTrue(l.hasGroupOption());
    assertTrue(l.hasAccOption());
    assertEquals("loop gang vector", l.getAccClauses());
    assertEquals("j1", l.getGroupName());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMappping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMappping());

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-extract");
    analyzeUnvalidClawLanguage("claw loop   -   extract ");
  }

  /**
   * Assert the result for valid loop extract CLAW directive
   * @param raw       Raw string valud of the CLAW directive to be analyzed.
   * @param induction Induction var to be found.
   * @param lower     Lower bound value to be found.
   * @param upper     Upper bound value to be found.
   * @param step      Step valu to be found if any.
   */
  private ClawLanguage analyzeValidClawLoopExtract(String raw, String induction,
                                           String lower, String upper,
                                           String step)
  {
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(ClawDirective.LOOP_EXTRACT, l.getDirective());
      assertEquals(induction, l.getRange().getInductionVar());
      assertEquals(lower, l.getRange().getLowerBound());
      assertEquals(upper, l.getRange().getUpperBound());
      if(step != null){
        assertEquals(step, l.getRange().getStep());
      }
      return l;
    } catch(IllegalDirectiveException idex){
      System.err.println(idex.getMessage());
      fail();
      return null;
    }
  }

  /**
   * Test various input for the CLAW kcache directive.
   */
  @Test
  public void KcacheTest(){
    // Valid directives
    analyzeValidKcache("claw kcache", null);
    analyzeValidKcache("claw kcache 0 1", Arrays.asList("0", "1"));
    analyzeValidKcache("claw kcache 0 -1 0", Arrays.asList("0", "-1", "0"));
    analyzeValidKcache("claw kcache +1 -1 0", Arrays.asList("1", "-1", "0"));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw k cache ");
    analyzeUnvalidClawLanguage("claw k-cache");
  }

  /**
   * Assert the result for valid lo CLAW directive
   * @param raw       Raw string valud of the CLAW directive to be analyzed.
   * @param offsets   List of offsets to be checked.
   */
  private void analyzeValidKcache(String raw, List<String> offsets) {
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(ClawDirective.KCACHE, l.getDirective());
      if(offsets != null){
        assertEquals(offsets.size(), l.getOffsets().size());
        for(int i = 0; i < offsets.size(); ++i){
          assertEquals(offsets.get(i), l.getOffsets().get(i));
        }
      }
    } catch(IllegalDirectiveException idex){
      System.err.print(idex.getMessage());
      fail();
    }
  }

  /**
   * Test various input for the CLAW array-transform directive.
   */
  @Test
  public void ArrayTransformTest(){
    // Valid directives
    analyzeValidArrayTransform("claw array-transform", false, null, false,
        null);
    analyzeValidArrayTransform("claw array-transform fusion", true, null, false,
        null);
    analyzeValidArrayTransform("claw array-transform fusion group(j1)", true,
        "j1", false, null);
    analyzeValidArrayTransform("claw array-transform fusion parallel", true,
        null, true, null);
    analyzeValidArrayTransform("claw array-transform fusion parallel acc(loop)",
        true, null, true, "loop");
    analyzeValidArrayTransform("claw array-transform fusion acc(loop)", true,
        null, false, "loop");
    analyzeValidArrayTransform(
        "claw array-transform fusion parallel acc(loop gang vector)", true,
        null, true, "loop gang vector");
    analyzeValidArrayTransform(
        "claw array-transform fusion group(j1) parallel acc(loop gang vector)",
        true, "j1", true, "loop gang vector");
    analyzeValidArrayTransform(
        "claw array-transform parallel acc(loop gang vector)",
        false, null, true, "loop gang vector");
    analyzeValidArrayTransform("claw array-transform parallel", false, null,
        true, null);
    analyzeValidArrayTransform("claw array-transform acc(loop gang vector)",
        false, null, false, "loop gang vector");
  }

  /**
   * Assert the result for valid lo CLAW directive
   * @param raw         Raw string valud of the CLAW directive to be analyzed.
   * @param fusion      Set to true if the extracted option should be present.
   * @param fusionGroup Name of the group in the extracted fusion option.
   * @param parallel    Set to true if the extracted option should be present.
   * @param acc         String of acc clauses that should be present.
   */
  private void analyzeValidArrayTransform(String raw, boolean fusion,
                                          String fusionGroup, boolean parallel,
                                          String acc)
  {
    try {
      ClawLanguage l = ClawLanguage.analyze(raw);
      assertEquals(ClawDirective.ARRAY_TRANSFORM, l.getDirective());
      if(fusion){
        assertTrue(l.hasFusionOption());
        assertEquals(fusionGroup, l.getGroupName());
      } else {
        assertFalse(l.hasFusionOption());
        assertNull(l.getGroupName());
      }
      if(parallel){
        assertTrue(l.hasParallelOption());
      } else {
        assertFalse(l.hasParallelOption());
      }
      if(acc != null){
        assertTrue(l.hasAccOption());
        assertEquals(acc, l.getAccClauses());
      } else {
        assertFalse(l.hasAccOption());
        assertNull(acc);
      }
    } catch(IllegalDirectiveException idex){
      System.err.print(idex.getMessage());
      fail();
    }
  }

}
