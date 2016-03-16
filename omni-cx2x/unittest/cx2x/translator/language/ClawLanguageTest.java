/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import static org.junit.Assert.*;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import org.junit.Test;

import java.util.ArrayList;
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
    }
  }

  /**
   * Test various input for the CLAW loop interchange directive.
   */
  @Test
  public void InterchangeTest(){
    // Valid directives
    analyzeValidClawLoopInterchange("claw loop-interchange", null);
    analyzeValidClawLoopInterchange("claw loop-interchange (i,j,k)", Arrays.asList("i", "j", "k"));

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

}
