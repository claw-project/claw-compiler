/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language;

import cx2x.translator.config.Configuration;
import cx2x.translator.language.base.ClawDMD;
import cx2x.translator.language.base.ClawDirective;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawDimension;
import cx2x.translator.language.common.ClawMapping;
import cx2x.translator.language.common.ClawReshapeInfo;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorGenerator;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.language.helper.target.Target;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.xnode.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

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
  public void FusionTest() {
    // Valid directives
    analyzeValidClawLoopFusion("claw loop-fusion", null, false, 0, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1)", "g1", false, 0,
        null);
    analyzeValidClawLoopFusion("claw loop-fusion group( g1 )", "g1", false, 0,
        null);
    analyzeValidClawLoopFusion("claw loop-fusion group ( g1   ) ", "g1",
        false, 0, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) collapse(2)", "g1",
        true, 2, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) collapse(3)", "g1",
        true, 3, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(3) group(g1)", "g1",
        true, 3, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(2)", null,
        true, 2, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(3)", null,
        true, 3, null);

    // With target clause
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu)", null, false, 0,
        Collections.singletonList(Target.CPU));
    analyzeValidClawLoopFusion("claw loop-fusion target(gpu)", null, false, 0,
        Collections.singletonList(Target.GPU));
    analyzeValidClawLoopFusion("claw loop-fusion target(mic)", null, false, 0,
        Collections.singletonList(Target.MIC));
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,cpu)", null, false,
        0, Collections.singletonList(Target.CPU));
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu)", null, false,
        0, Arrays.asList(Target.CPU, Target.GPU));
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu,mic)", null,
        false, 0, Arrays.asList(Target.CPU, Target.GPU, Target.MIC));
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu) collapse(3) " +
        "group(g1)", "g1", true, 3, Arrays.asList(Target.CPU, Target.GPU));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-fusiongroup(g1)");
    analyzeUnvalidClawLanguage("claw loop-fusion group");
    analyzeUnvalidClawLanguage("claw loop-fusion (i,j,k)");
    analyzeUnvalidClawLanguage("claw loop-fusion group()");
    analyzeUnvalidClawLanguage("claw loop-fusion group(   )");
  }

  /**
   * Assert the result for valid loop fusion CLAW directive
   *
   * @param raw       Raw string value of the CLAW directive to be analyzed.
   * @param groupName Group name to be found if any.
   */
  private void analyzeValidClawLoopFusion(String raw, String groupName,
                                          boolean collapse, int n,
                                          List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.LOOP_FUSION, l.getDirective());
      if(groupName != null) {
        assertTrue(l.hasGroupClause());
        assertEquals(groupName, l.getGroupValue());
      } else {
        assertFalse(l.hasGroupClause());
        assertNull(l.getGroupValue());
      }
      if(collapse) {
        assertTrue(l.hasCollapseClause());
        assertEquals(n, l.getCollapseValue());
      } else {
        assertFalse(l.hasCollapseClause());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      fail();
    }
  }

  /**
   * Assert any unvalid claw raw input
   *
   * @param raw Raw string value of the CLAW directive to be analyzed.
   */
  private void analyzeUnvalidClawLanguage(String raw) {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage.analyze(p, generator, Target.GPU);
      fail();
    } catch(IllegalDirectiveException pex) {
      assertNotNull(pex);
      assertNotNull(pex.getMessage());
    }
  }

  /**
   * Test various input for the CLAW loop interchange directive.
   */
  @Test
  public void InterchangeTest() {
    // Valid directives
    analyzeValidClawLoopInterchange("claw loop-interchange", null, false, null,
        null);
    analyzeValidClawLoopInterchange("claw loop-interchange (i,j,k)",
        Arrays.asList("i", "j", "k"), false, null, null);
    analyzeValidClawLoopInterchange("claw loop-interchange (  i,j,k  ) ",
        Arrays.asList("i", "j", "k"), false, null, null);

    analyzeValidClawLoopInterchange("claw loop-interchange parallel",
        null, true, null, null);
    analyzeValidClawLoopInterchange("claw loop-interchange parallel acc(loop)",
        null, true, "loop", null);
    analyzeValidClawLoopInterchange("claw loop-interchange acc(loop) parallel",
        null, true, "loop", null);
    analyzeValidClawLoopInterchange("claw loop-interchange acc(loop)", null,
        false, "loop", null);
    analyzeValidClawLoopInterchange("claw loop-interchange (j,k,i) parallel",
        Arrays.asList("j", "k", "i"), true, null, null);


    analyzeValidClawLoopInterchange("claw loop-interchange target(cpu)",
        null, false, null, Collections.singletonList(Target.CPU));
    analyzeValidClawLoopInterchange("claw loop-interchange target(cpu,mic)",
        null, false, null, Arrays.asList(Target.CPU, Target.MIC));
    analyzeValidClawLoopInterchange("claw loop-interchange target(cpu) " +
            "parallel acc(loop)", null, true, "loop",
        Collections.singletonList(Target.CPU));


    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-interchange ()");
    analyzeUnvalidClawLanguage("claw loop-interchange (i,j,k) group");
  }

  /**
   * Assert the result for valid loop interchange CLAW directive
   *
   * @param raw     Raw string value of the CLAW directive to be analyzed.
   * @param indexes List of indexes to be found if any.
   */
  private void analyzeValidClawLoopInterchange(String raw,
                                               List<String> indexes,
                                               boolean parallel, String acc,
                                               List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.LOOP_INTERCHANGE, l.getDirective());
      if(indexes != null) {
        assertTrue(l.hasIndexes());
        assertEquals(indexes.size(), l.getIndexes().size());
      } else {
        assertFalse(l.hasIndexes());
        assertNull(l.getIndexes());
      }

      if(parallel) {
        assertTrue(l.hasParallelClause());
      } else {
        assertFalse(l.hasParallelClause());
      }

      if(acc != null) {
        assertTrue(l.hasAcceleratorClause());
        assertEquals(acc, l.getAcceleratorClauses());
      } else {
        assertFalse(l.hasAcceleratorClause());
        assertNull(l.getAcceleratorClauses());
      }

      assertTargets(l, targets);

    } catch(IllegalDirectiveException idex) {
      fail();
    }
  }

  /**
   * Assert the information sorted in the target list.
   *
   * @param l       The current ClawLanguage object.
   * @param targets List of expected targets.
   */
  private void assertTargets(ClawLanguage l, List<Target> targets) {
    if(targets != null) {
      assertTrue(l.hasTargetClause());
      assertEquals(targets.size(), l.getTargetClauseValues().size());
      for(Target t : targets) {
        assertTrue(l.getTargetClauseValues().contains(t));
      }
    } else {
      assertFalse(l.hasTargetClause());
    }
  }

  /**
   * Test various input for the CLAW remove directive.
   */
  @Test
  public void RemoveTest() {
    // Valid directives
    analyzeValidSimpleClaw("claw remove", ClawDirective.REMOVE, false, null);
    analyzeValidSimpleClaw("claw end remove", ClawDirective.REMOVE, true, null);
    analyzeValidSimpleClaw("claw   end   remove  ", ClawDirective.REMOVE, true,
        null);

    analyzeValidSimpleClaw("claw remove target(cpu)", ClawDirective.REMOVE,
        false, Collections.singletonList(Target.CPU));
    analyzeValidSimpleClaw("claw remove target(mic)", ClawDirective.REMOVE,
        false, Collections.singletonList(Target.MIC));
    analyzeValidSimpleClaw("claw remove target(gpu,mic)", ClawDirective.REMOVE,
        false, Arrays.asList(Target.GPU, Target.MIC));


    // Unvalid directives
    analyzeUnvalidClawLanguage("claw");
    analyzeUnvalidClawLanguage("claw dummy");
    analyzeUnvalidClawLanguage("claw end re move");
  }

  /**
   * Assert the result for valid simple CLAW directive
   *
   * @param raw       Raw string value of the CLAW directive to be analyzed.
   * @param directive Directive to be match.
   */
  private void analyzeValidSimpleClaw(String raw, ClawDirective directive,
                                      boolean isEnd, List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(directive, l.getDirective());
      if(isEnd) {
        assertTrue(l.isEndPragma());
      } else {
        assertFalse(l.isEndPragma());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      fail();
    }
  }


  /**
   * Test various input for the CLAW loop extract directive.
   */
  @Test
  public void ExtractTest() {
    // Valid directives
    ClawLanguage l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j)", "i", "istart",
        "iend", null, null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    ClawMapping map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend,2) map(i:j)", "i", "istart",
        "iend", "2", null);
    assertNotNull(l);
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract("claw loop-extract range(i=1,10) map(i:j)",
        "i", "1", "10", null, null);
    assertNotNull(l);
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=1,10,2) map(i:j) parallel",
        "i", "1", "10", "2", null);
    assertNotNull(l);
    map = l.getMappings().get(0);
    assertTrue(l.hasParallelClause());
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion", "i", "istart",
        "iend", null, null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionClause());
    assertFalse(l.hasGroupClause());
    assertFalse(l.hasParallelClause());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());


    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion group(j1)",
        "i", "istart", "iend", null, null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionClause());
    assertTrue(l.hasGroupClause());
    assertEquals("j1", l.getGroupValue());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion group(j1) " +
            "acc(loop gang vector)", "i", "istart", "iend", null, null);
    assertNotNull(l);
    assertEquals(1, l.getMappings().size());
    assertNotNull(l.getMappings().get(0));
    assertTrue(l.hasFusionClause());
    assertTrue(l.hasGroupClause());
    assertTrue(l.hasAcceleratorClause());
    assertEquals("loop gang vector", l.getAcceleratorClauses());
    assertEquals("j1", l.getGroupValue());
    map = l.getMappings().get(0);
    assertEquals(1, map.getMappedVariables().size());
    assertEquals(1, map.getMappingVariables().size());
    assertEquals("i", map.getMappedVariables().get(0).getArgMapping());
    assertEquals("i", map.getMappedVariables().get(0).getFctMapping());
    assertFalse(map.getMappedVariables().get(0).hasDifferentMapping());
    assertEquals("j", map.getMappingVariables().get(0).getArgMapping());
    assertEquals("j", map.getMappingVariables().get(0).getFctMapping());
    assertFalse(map.getMappingVariables().get(0).hasDifferentMapping());

    l = analyzeValidClawLoopExtract(
        "claw loop-extract range(j1=ki1sc,ki1ec) " +
            "map(pduh2oc,pduh2of:j1,ki3sc/j3) " +
            "map(pduco2,pduo3,palogp,palogt,podsc,podsf,podac,podaf:j1,ki3sc/j3) " +
            "map(pbsff,pbsfc:j1,ki3sc/j3) map(pa1c,pa1f,pa2c,pa2f,pa3c,pa3f:j1) " +
            "fusion group(coeth-j1) parallel acc(loop gang vector)",
        "j1", "ki1sc", "ki1ec", null, null);
    assertNotNull(l);
    assertEquals(4, l.getMappings().size());

    ClawMapping map1 = l.getMappings().get(0);
    assertNotNull(map1);
    assertEquals(2, map1.getMappedVariables().size());
    assertEquals(2, map1.getMappingVariables().size());
    assertEquals("pduh2oc", map1.getMappedVariables().get(0).getArgMapping());
    assertEquals("pduh2oc", map1.getMappedVariables().get(0).getFctMapping());
    assertEquals("pduh2of", map1.getMappedVariables().get(1).getArgMapping());
    assertEquals("pduh2of", map1.getMappedVariables().get(1).getFctMapping());
    assertEquals("j1", map1.getMappingVariables().get(0).getArgMapping());
    assertEquals("j1", map1.getMappingVariables().get(0).getFctMapping());
    assertEquals("ki3sc", map1.getMappingVariables().get(1).getArgMapping());
    assertEquals("j3", map1.getMappingVariables().get(1).getFctMapping());

    ClawMapping map2 = l.getMappings().get(1);
    assertNotNull(map2);
    assertEquals(8, map2.getMappedVariables().size());
    assertEquals(2, map2.getMappingVariables().size());
    assertEquals("pduco2", map2.getMappedVariables().get(0).getArgMapping());
    assertEquals("pduco2", map2.getMappedVariables().get(0).getFctMapping());
    assertEquals("pduo3", map2.getMappedVariables().get(1).getArgMapping());
    assertEquals("pduo3", map2.getMappedVariables().get(1).getFctMapping());
    assertEquals("palogp", map2.getMappedVariables().get(2).getArgMapping());
    assertEquals("palogp", map2.getMappedVariables().get(2).getFctMapping());
    assertEquals("palogt", map2.getMappedVariables().get(3).getArgMapping());
    assertEquals("palogt", map2.getMappedVariables().get(3).getFctMapping());
    assertEquals("podsc", map2.getMappedVariables().get(4).getArgMapping());
    assertEquals("podsc", map2.getMappedVariables().get(4).getFctMapping());
    assertEquals("podsf", map2.getMappedVariables().get(5).getArgMapping());
    assertEquals("podsf", map2.getMappedVariables().get(5).getFctMapping());
    assertEquals("podac", map2.getMappedVariables().get(6).getArgMapping());
    assertEquals("podac", map2.getMappedVariables().get(6).getFctMapping());
    assertEquals("podaf", map2.getMappedVariables().get(7).getArgMapping());
    assertEquals("podaf", map2.getMappedVariables().get(7).getFctMapping());
    assertEquals("j1", map2.getMappingVariables().get(0).getArgMapping());
    assertEquals("j1", map2.getMappingVariables().get(0).getFctMapping());
    assertEquals("ki3sc", map2.getMappingVariables().get(1).getArgMapping());
    assertEquals("j3", map2.getMappingVariables().get(1).getFctMapping());

    ClawMapping map3 = l.getMappings().get(2);
    assertNotNull(map3);
    assertEquals(2, map3.getMappedVariables().size());
    assertEquals(2, map3.getMappingVariables().size());
    assertEquals("pbsff", map3.getMappedVariables().get(0).getArgMapping());
    assertEquals("pbsff", map3.getMappedVariables().get(0).getFctMapping());
    assertEquals("pbsfc", map3.getMappedVariables().get(1).getArgMapping());
    assertEquals("pbsfc", map3.getMappedVariables().get(1).getFctMapping());
    assertEquals("j1", map3.getMappingVariables().get(0).getArgMapping());
    assertEquals("j1", map3.getMappingVariables().get(0).getFctMapping());
    assertEquals("ki3sc", map3.getMappingVariables().get(1).getArgMapping());
    assertEquals("j3", map3.getMappingVariables().get(1).getFctMapping());

    ClawMapping map4 = l.getMappings().get(3);
    assertNotNull(map4);
    assertEquals(6, map4.getMappedVariables().size());
    assertEquals(1, map4.getMappingVariables().size());
    assertEquals("pa1c", map4.getMappedVariables().get(0).getArgMapping());
    assertEquals("pa1c", map4.getMappedVariables().get(0).getFctMapping());
    assertEquals("pa1f", map4.getMappedVariables().get(1).getArgMapping());
    assertEquals("pa1f", map4.getMappedVariables().get(1).getFctMapping());
    assertEquals("pa2c", map4.getMappedVariables().get(2).getArgMapping());
    assertEquals("pa2c", map4.getMappedVariables().get(2).getFctMapping());
    assertEquals("pa2f", map4.getMappedVariables().get(3).getArgMapping());
    assertEquals("pa2f", map4.getMappedVariables().get(3).getFctMapping());
    assertEquals("pa3c", map4.getMappedVariables().get(4).getArgMapping());
    assertEquals("pa3c", map4.getMappedVariables().get(4).getFctMapping());
    assertEquals("pa3f", map4.getMappedVariables().get(5).getArgMapping());
    assertEquals("pa3f", map4.getMappedVariables().get(5).getFctMapping());
    assertEquals("j1", map4.getMappingVariables().get(0).getArgMapping());
    assertEquals("j1", map4.getMappingVariables().get(0).getFctMapping());

    assertTrue(l.hasFusionClause());
    assertTrue(l.hasGroupClause());
    assertEquals("coeth-j1", l.getGroupValue());
    assertTrue(l.hasAcceleratorClause());
    assertEquals("loop gang vector", l.getAcceleratorClauses());


    analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) target(gpu) fusion " +
            "group(j1)",
        "i", "istart", "iend", null, Collections.singletonList(Target.GPU));

    analyzeValidClawLoopExtract(
        "claw loop-extract range(i=istart,iend) map(i:j) fusion group(j1) " +
            "target(gpu)",
        "i", "istart", "iend", null, Collections.singletonList(Target.GPU));


    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-extract");
    analyzeUnvalidClawLanguage("claw loop   -   extract ");
  }

  /**
   * Assert the result for valid loop extract CLAW directive
   *
   * @param raw       Raw string value of the CLAW directive to be analyzed.
   * @param induction Induction var to be found.
   * @param lower     Lower bound value to be found.
   * @param upper     Upper bound value to be found.
   * @param step      Step valu to be found if any.
   */
  private ClawLanguage analyzeValidClawLoopExtract(String raw, String induction,
                                                   String lower, String upper,
                                                   String step,
                                                   List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.LOOP_EXTRACT, l.getDirective());
      assertEquals(induction, l.getRange().getInductionVar());
      assertEquals(lower, l.getRange().getLowerBound());
      assertEquals(upper, l.getRange().getUpperBound());
      if(step != null) {
        assertEquals(step, l.getRange().getStep());
      }
      assertTargets(l, targets);
      return l;
    } catch(IllegalDirectiveException idex) {
      System.err.println(idex.getMessage());
      fail();
      return null;
    }
  }

  /**
   * Test various input for the CLAW kcache directive.
   */
  @Test
  public void KcacheTest() {

    // data clause + offsets
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, 1), false, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,-1,0)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, -1, 0), false, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0)",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), false, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0) private",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), false, true, null);
    analyzeValidKcache("claw kcache data(var1,var2) private",
        Arrays.asList("var1", "var2"), null, false, true, null);

    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1) target(cpu)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, 1), false, false,
        Collections.singletonList(Target.CPU));
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1) target(gpu)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, 1), false, false,
        Collections.singletonList(Target.GPU));
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1) " +
            "target(cpu,gpu)", Arrays.asList("var1", "var2"),
        Arrays.asList(0, 1), false, false,
        Arrays.asList(Target.CPU, Target.GPU));

    // offset + init clause
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1) init",
        Arrays.asList("var1", "var2"), Arrays.asList(0, 1), true, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,-1,0) init",
        Arrays.asList("var1", "var2"), Arrays.asList(0, -1, 0), true, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0) init",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), true, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0) init private",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), true, true, null);
    analyzeValidKcache("claw kcache data(var1,var2) init private",
        Arrays.asList("var1", "var2"), null, true, true, null);

    analyzeValidKcache("claw kcache data(var1,var2) init target(cpu) private",
        Arrays.asList("var1", "var2"), null, true, true,
        Collections.singletonList(Target.CPU));

    analyzeValidKcache("claw kcache data(var1,var2) target(gpu) init private",
        Arrays.asList("var1", "var2"), null, true, true,
        Collections.singletonList(Target.GPU));

    analyzeValidKcache("claw kcache data(var1,var2) init private target(gpu)",
        Arrays.asList("var1", "var2"), null, true, true,
        Collections.singletonList(Target.GPU));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw k cache ");
    analyzeUnvalidClawLanguage("claw k-cache");
  }

  /**
   * Assert the result for valid CLAW kcache directive
   *
   * @param raw     Raw string value of the CLAW directive to be analyzed.
   * @param data    List of identifiers to be checked.
   * @param offsets List of offsets to be checked.
   * @param init    If true, check that ClawLanguage object has init clause
   *                enabled.
   */
  private void analyzeValidKcache(String raw, List<String> data,
                                  List<Integer> offsets, boolean init,
                                  boolean hasPrivate, List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.KCACHE, l.getDirective());
      if(data != null) {
        assertTrue(l.hasDataClause());
        assertEquals(data.size(), l.getDataClauseValues().size());
        for(int i = 0; i < data.size(); ++i) {
          assertEquals(data.get(i), l.getDataClauseValues().get(i));
        }
      } else {
        assertFalse(l.hasDataClause());
      }
      if(offsets != null) {
        assertEquals(offsets.size(), l.getOffsets().size());
        for(int i = 0; i < offsets.size(); ++i) {
          assertEquals(offsets.get(i), l.getOffsets().get(i));
        }
      }
      if(init) {
        assertTrue(l.hasInitClause());
      } else {
        assertFalse(l.hasInitClause());
      }
      if(hasPrivate) {
        assertTrue(l.hasPrivateClause());
      } else {
        assertFalse(l.hasPrivateClause());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
  }

  /**
   * Test various input for the CLAW array-transform directive.
   */
  @Test
  public void ArrayTransformTest() {
    // Valid directives
    analyzeValidArrayTransform("claw array-transform", false, null, false,
        null, null, null);
    analyzeValidArrayTransform("claw array-transform fusion", true, null, false,
        null, null, null);
    analyzeValidArrayTransform("claw array-transform fusion group(j1)", true,
        "j1", false, null, null, null);
    analyzeValidArrayTransform("claw array-transform fusion parallel", true,
        null, true, null, null, null);
    analyzeValidArrayTransform("claw array-transform fusion parallel acc(loop)",
        true, null, true, "loop", null, null);
    analyzeValidArrayTransform("claw array-transform fusion acc(loop)", true,
        null, false, "loop", null, null);
    analyzeValidArrayTransform(
        "claw array-transform fusion parallel acc(loop gang vector)", true,
        null, true, "loop gang vector", null, null);
    analyzeValidArrayTransform(
        "claw array-transform fusion group(j1) parallel acc(loop gang vector)",
        true, "j1", true, "loop gang vector", null, null);
    analyzeValidArrayTransform(
        "claw array-transform parallel acc(loop gang vector)",
        false, null, true, "loop gang vector", null, null);
    analyzeValidArrayTransform("claw array-transform parallel", false, null,
        true, null, null, null);
    analyzeValidArrayTransform("claw array-transform acc(loop gang vector)",
        false, null, false, "loop gang vector", null, null);

    analyzeValidArrayTransform("claw array-transform induction(j1,j3)",
        false, null, false, null, Arrays.asList("j1", "j3"), null);
    analyzeValidArrayTransform("claw array-transform induction(j1)",
        false, null, false, null, Collections.singletonList("j1"), null);
    analyzeValidArrayTransform("claw array-transform induction(i,j,k)",
        false, null, false, null, Arrays.asList("i", "j", "k"), null);
    analyzeUnvalidClawLanguage("claw array-transform induction()");
    analyzeUnvalidClawLanguage("claw array-transform induction");


    analyzeValidArrayTransform("claw array-transform target(cpu)", false, null,
        false, null, null, Collections.singletonList(Target.CPU));
    analyzeValidArrayTransform("claw array-transform target(gpu)", false, null,
        false, null, null, Collections.singletonList(Target.GPU));
    analyzeValidArrayTransform("claw array-transform target(cpu, gpu)", false,
        null, false, null, null, Arrays.asList(Target.CPU, Target.GPU));

    analyzeValidArrayTransform(
        "claw array-transform target(cpu) fusion parallel acc(loop)",
        true, null, true, "loop", null, Collections.singletonList(Target.CPU));
    analyzeValidArrayTransform(
        "claw array-transform fusion target(cpu) parallel acc(loop)",
        true, null, true, "loop", null, Collections.singletonList(Target.CPU));
    analyzeValidArrayTransform(
        "claw array-transform fusion parallel target(cpu) acc(loop)",
        true, null, true, "loop", null, Collections.singletonList(Target.CPU));
    analyzeValidArrayTransform(
        "claw array-transform fusion parallel acc(loop) target(cpu)",
        true, null, true, "loop", null, Collections.singletonList(Target.CPU));

    analyzeValidSimpleClaw("claw end array-transform",
        ClawDirective.ARRAY_TRANSFORM, true, null);
    analyzeValidSimpleClaw("claw   end   array-transform  ",
        ClawDirective.ARRAY_TRANSFORM, true, null);
  }

  /**
   * Assert the result for valid CLAW array-transform directive
   *
   * @param raw         Raw string value of the CLAW directive to be analyzed.
   * @param fusion      Set to true if the extracted option should be present.
   * @param fusionGroup Name of the group in the extracted fusion option.
   * @param parallel    Set to true if the extracted option should be present.
   * @param acc         String of acc clauses that should be present.
   */
  private void analyzeValidArrayTransform(String raw, boolean fusion,
                                          String fusionGroup, boolean parallel,
                                          String acc, List<String> inducNames,
                                          List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.ARRAY_TRANSFORM, l.getDirective());
      if(fusion) {
        assertTrue(l.hasFusionClause());
        assertEquals(fusionGroup, l.getGroupValue());
      } else {
        assertFalse(l.hasFusionClause());
        assertNull(l.getGroupValue());
      }
      if(parallel) {
        assertTrue(l.hasParallelClause());
      } else {
        assertFalse(l.hasParallelClause());
      }
      if(acc != null) {
        assertTrue(l.hasAcceleratorClause());
        assertEquals(acc, l.getAcceleratorClauses());
      } else {
        assertFalse(l.hasAcceleratorClause());
      }
      if(inducNames != null) {
        assertTrue(l.hasInductionClause());
        assertEquals(inducNames.size(), l.getInductionValues().size());
        for(int i = 0; i < inducNames.size(); ++i) {
          assertEquals(inducNames.get(i), l.getInductionValues().get(i));
        }
      } else {
        assertFalse(l.hasInductionClause());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
  }


  /**
   * Test various input for the CLAW loop-hoist directive.
   */
  @Test
  public void LoopHoistTest() {
    // Valid directives
    analyzeValidLoopHoist("claw loop-hoist(i,j)",
        Arrays.asList("i", "j"), false, null, false, null, null);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange",
        Arrays.asList("i", "j"), true, null, false, null, null);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange(j,i)",
        Arrays.asList("i", "j"), true, Arrays.asList("j", "i"), false, null,
        null);

    List<Integer> empty = Collections.emptyList();
    ClawReshapeInfo info1 = new ClawReshapeInfo("zmd", 0, empty);
    ClawReshapeInfo info2 =
        new ClawReshapeInfo("zsediflux", 1, Collections.singletonList(2));
    analyzeValidLoopHoist("claw loop-hoist(i,j) reshape(zmd(0), zsediflux(1,2))",
        Arrays.asList("i", "j"), false, null, true,
        Arrays.asList(info1, info2), null);


    analyzeValidLoopHoist("claw loop-hoist(i,j) target(cpu) interchange",
        Arrays.asList("i", "j"), true, null, false, null,
        Collections.singletonList(Target.CPU));

    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange target(cpu, gpu)",
        Arrays.asList("i", "j"), true, null, false, null,
        Arrays.asList(Target.CPU, Target.GPU));

    analyzeValidLoopHoist("claw loop-hoist(i,j) target(mic)",
        Arrays.asList("i", "j"), false, null, false, null,
        Collections.singletonList(Target.MIC));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw loop-hoist");
    analyzeUnvalidClawLanguage("claw loop-hoist()");
    analyzeUnvalidClawLanguage("claw loop-hoist(i,j) interchange()");

    analyzeValidSimpleClaw("claw end loop-hoist",
        ClawDirective.LOOP_HOIST, true, null);
    analyzeValidSimpleClaw("claw   end   loop-hoist  ",
        ClawDirective.LOOP_HOIST, true, null);
  }

  /**
   * Assert the result for valid CLAW loop-hoist directive
   *
   * @param raw        Raw string value of the CLAW directive to be analyzed.
   * @param inductions List of induction variables to be checked.
   */
  private void analyzeValidLoopHoist(String raw, List<String> inductions,
                                     boolean interchange, List<String> indexes,
                                     boolean reshape,
                                     List<ClawReshapeInfo> infos,
                                     List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.LOOP_HOIST, l.getDirective());

      assertEquals(inductions.size(), l.getHoistInductionVars().size());
      for(int i = 0; i < inductions.size(); ++i) {
        assertEquals(inductions.get(i), l.getHoistInductionVars().get(i));
      }

      if(interchange) {
        assertTrue(l.hasInterchangeClause());
      } else {
        assertFalse(l.hasInterchangeClause());
      }

      if(indexes != null) {
        for(int i = 0; i < indexes.size(); ++i) {
          assertEquals(indexes.get(i), l.getIndexes().get(i));
        }
      }

      if(reshape) {
        assertTrue(l.hasReshapeClause());
        assertEquals(infos.size(), l.getReshapeClauseValues().size());
        for(int i = 0; i < infos.size(); ++i) {
          assertEquals(infos.get(i).getArrayName(),
              l.getReshapeClauseValues().get(i).getArrayName());
          assertEquals(infos.get(i).getTargetDimension(),
              l.getReshapeClauseValues().get(i).getTargetDimension());
          List<Integer> expected = infos.get(i).getKeptDimensions();
          List<Integer> actual =
              l.getReshapeClauseValues().get(i).getKeptDimensions();
          assertEquals(expected.size(), actual.size());
          for(int j = 0; j < expected.size(); ++j) {
            assertEquals(expected.get(j), actual.get(j));
          }
        }
      } else {
        assertFalse(l.hasReshapeClause());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
  }

  /**
   * Test various input for the CLAW call directive.
   */
  @Test
  public void ArrayToFctCallTest() {
    // Valid directives
    analyzeValidArrayToFctCall("claw call var1=f_var1(i,j)", "var1", "f_var1",
        Arrays.asList("i", "j"), null);

    analyzeValidArrayToFctCall("claw call var1=f_var1(i)", "var1", "f_var1",
        Collections.singletonList("i"), null);

    analyzeValidArrayToFctCall("claw call v=f(i,j)", "v", "f",
        Arrays.asList("i", "j"), null);

    analyzeValidArrayToFctCall("claw call v=f(i,j) target(cpu)", "v", "f",
        Arrays.asList("i", "j"), Collections.singletonList(Target.CPU));

    analyzeValidArrayToFctCall("claw call v=f(i,j) target(mic)", "v", "f",
        Arrays.asList("i", "j"), Collections.singletonList(Target.MIC));

    analyzeValidArrayToFctCall("claw call v=f(i,j) target(mic, gpu)", "v", "f",
        Arrays.asList("i", "j"), Arrays.asList(Target.MIC, Target.GPU));

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw call ");
    analyzeUnvalidClawLanguage("claw call v=");
    analyzeUnvalidClawLanguage("claw call v=()");
  }

  /**
   * Assert the result for valid CLAW call directive
   *
   * @param raw       Raw string value of the CLAW directive to be analyzed.
   * @param arrayName Array name to be checked.
   * @param fctName   Function name to be checked.
   * @param params    List of parameters identifier to be checked.
   */
  private void analyzeValidArrayToFctCall(String raw, String arrayName,
                                          String fctName, List<String> params,
                                          List<Target> targets)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.ARRAY_TO_CALL, l.getDirective());

      assertEquals(params.size(), l.getFctParams().size());
      for(int i = 0; i < params.size(); ++i) {
        assertEquals(params.get(i), l.getFctParams().get(i));
      }

      assertEquals(arrayName, l.getArrayName());
      assertEquals(fctName, l.getFctName());
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
  }

  /**
   * Test various input for the CLAW parallelize directive.
   */
  @Test
  public void parallelizeTest() {

    // Valid directives
    ClawDimension d1 = new ClawDimension("i", "1", "nx");
    List<String> data1 = Arrays.asList("t", "qc", "qv");
    List<List<String>> dataLst1 = Collections.singletonList(data1);
    List<String> ijc = Arrays.asList("i", "j", ":");
    List<String> icj = Arrays.asList("i", ":", "j");
    List<String> cij = Arrays.asList(":", "i", "j");
    List<List<String>> over1 = Collections.singletonList(ijc);
    List<List<String>> over2 = Collections.singletonList(icj);
    List<List<String>> over3 = Collections.singletonList(cij);

    analyzeValidParallelize("claw define dimension i(1:nx)" +
            " parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d1), null, null);

    ClawDimension d2 = new ClawDimension("j", "1", "ny");
    analyzeValidParallelize("claw define dimension j(1:ny)" +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d2), null, null);

    ClawDimension d3 = new ClawDimension("j", "1", "10");
    analyzeValidParallelize("claw define dimension j(1:10) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d3), null, null);

    ClawDimension d4 = new ClawDimension("j", "jstart", "10");
    analyzeValidParallelize("claw define dimension j(jstart:10) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d4), null, null);

    ClawDimension d5 = new ClawDimension("j", "jstart", "ny");
    analyzeValidParallelize("claw define dimension j(jstart:ny) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d5), null, null);

    ClawDimension d6 = new ClawDimension("j", "jstart", "ny");
    analyzeValidParallelize("claw define dimension j(jstart:ny) parallelize",
        null, null, Collections.singletonList(d6), null, null);

    analyzeValidParallelize("claw parallelize forward",
        null, null, null, null, null);

    analyzeValidParallelize("claw parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, null, null, null);

    analyzeValidParallelize("claw parallelize data(t,qc,qv) over (:,i,j)",
        dataLst1, over3, null, null, null);

    analyzeValidParallelize("claw parallelize data(t,qc,qv) over (i,:,j)",
        dataLst1, over2, null, null, null);

    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j)",
        dataLst1, over2, null, null, null);


    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "copy", dataLst1, over2, null, ClawDMD.BOTH, null);
    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "copy(in)", dataLst1, over2, null, ClawDMD.IN, null);
    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "copy(out)", dataLst1, over2, null, ClawDMD.OUT, null);

    ClawDimension d7 = new ClawDimension("c", "1", "nc");
    analyzeValidParallelize("claw define dimension c(1:nc) parallelize copy",
        null, null, Collections.singletonList(d7), ClawDMD.BOTH, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize copy(in)", null, null, Collections.singletonList(d7),
        ClawDMD.IN, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize copy(out)", null, null, Collections.singletonList(d7),
        ClawDMD.OUT, null);

    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "update", dataLst1, over2, null, null, ClawDMD.BOTH);
    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "update(in)", dataLst1, over2, null, null, ClawDMD.IN);
    analyzeValidParallelize("claw parallelize data(t , qc , qv) over (i,:,j) " +
        "update(out)", dataLst1, over2, null, null, ClawDMD.OUT);

    analyzeValidParallelize("claw define dimension c(1:nc) parallelize update",
        null, null, Collections.singletonList(d7), null, ClawDMD.BOTH);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize update(in)", null, null, Collections.singletonList(d7),
        null, ClawDMD.IN);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize update(out)", null, null, Collections.singletonList(d7),
        null, ClawDMD.OUT);

    analyzeValidParallelize("claw parallelize forward copy",
        null, null, null, ClawDMD.BOTH, null);
    analyzeValidParallelize("claw parallelize forward copy(in)",
        null, null, null, ClawDMD.IN, null);
    analyzeValidParallelize("claw parallelize forward copy(out)",
        null, null, null, ClawDMD.OUT, null);

    analyzeValidParallelize("claw parallelize forward update",
        null, null, null, null, ClawDMD.BOTH);
    analyzeValidParallelize("claw parallelize forward update(in)",
        null, null, null, null, ClawDMD.IN);
    analyzeValidParallelize("claw parallelize forward update(out)",
        null, null, null, null, ClawDMD.OUT);

    List<String> data2 = Collections.singletonList("t");
    List<String> data3 = Collections.singletonList("q");
    List<List<String>> dataLst2 = Arrays.asList(data2, data3);

    List<String> ic = Arrays.asList("i", ":");
    List<String> ci = Arrays.asList(":", "i");
    List<List<String>> over4 = Arrays.asList(ic, ci);

    analyzeValidParallelize("claw parallelize data(t) over (i,:) data(q) " +
        "over(:,i)", dataLst2, over4, null, null, null);

    // Unvalid directives
    analyzeUnvalidClawLanguage("claw parallelize data over ");
    analyzeUnvalidClawLanguage("claw parallelize data");
    analyzeUnvalidClawLanguage("claw parallelize over");
    analyzeUnvalidClawLanguage("claw parallelite data() over ()");
  }

  /**
   * Assert the result for valid CLAW parallelize directive
   *
   * @param raw          Raw string value of the CLAW directive to be analyzed.
   * @param data         Reference list for the data clause values.
   * @param over         Reference list for the over clause values.
   * @param dimensions   Reference list of dimensions.
   * @param copyClause   Expected value for copy clause (Null if no copy clause)
   * @param updateClause Expected value for update clause
   *                     (Null if no update clause)
   */
  private void analyzeValidParallelize(String raw, List<List<String>> data,
                                       List<List<String>> over,
                                       List<ClawDimension> dimensions,
                                       ClawDMD copyClause, ClawDMD updateClause)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration configuration =
          new Configuration(AcceleratorDirective.OPENACC, Target.GPU);
      AcceleratorGenerator generator =
          AcceleratorHelper.createAcceleratorGenerator(configuration);
      ClawLanguage l = ClawLanguage.analyze(p, generator, Target.GPU);
      assertEquals(ClawDirective.PARALLELIZE, l.getDirective());

      if(data != null) {
        assertTrue(l.hasOverDataClause());
        assertEquals(data.size(), l.getOverDataClauseValues().size());
        for(int i = 0; i < data.size(); ++i) {
          assertEquals(data.get(i).size(),
              l.getOverDataClauseValues().get(i).size());
          for(int j = 0; j < data.get(i).size(); ++j) {
            assertEquals(data.get(i).get(j),
                l.getOverDataClauseValues().get(i).get(j));
          }
        }
      }

      if(over != null) {
        assertTrue(l.hasOverClause());
        assertEquals(over.size(), l.getOverClauseValues().size());
        for(int i = 0; i < over.size(); ++i) {
          assertEquals(over.get(i).size(),
              l.getOverClauseValues().get(i).size());
          for(int j = 0; j < over.get(i).size(); ++j) {
            assertEquals(over.get(i).get(j),
                l.getOverClauseValues().get(i).get(j));
          }
        }
      }

      if(dimensions != null) {
        assertEquals(dimensions.size(), l.getDimensionValues().size());
        for(int i = 0; i < dimensions.size(); ++i) {
          assertEquals(dimensions.get(i).getIdentifier(),
              l.getDimensionValues().get(i).getIdentifier());
          assertEquals(dimensions.get(i).lowerBoundIsVar(),
              l.getDimensionValues().get(i).lowerBoundIsVar());
          assertEquals(dimensions.get(i).upperBoundIsVar(),
              l.getDimensionValues().get(i).upperBoundIsVar());
          assertEquals(dimensions.get(i).getLowerBoundInt(),
              l.getDimensionValues().get(i).getLowerBoundInt());
          assertEquals(dimensions.get(i).getUpperBoundInt(),
              l.getDimensionValues().get(i).getUpperBoundInt());
          assertEquals(dimensions.get(i).getLowerBoundId(),
              l.getDimensionValues().get(i).getLowerBoundId());
          assertEquals(dimensions.get(i).getUpperBoundId(),
              l.getDimensionValues().get(i).getUpperBoundId());
        }
      }

      if(data == null && over == null && dimensions == null) {
        assertTrue(l.hasForwardClause());
      }

      if(copyClause == null) {
        assertFalse(l.hasCopyClause());
        assertNull(l.getCopyClauseValue());
      } else {
        assertTrue(l.hasCopyClause());
        assertEquals(copyClause, l.getCopyClauseValue());
      }

      if(updateClause == null) {
        assertFalse(l.hasUpdateClause());
        assertNull(l.getUpdateClauseValue());
      } else {
        assertTrue(l.hasUpdateClause());
        assertEquals(updateClause, l.getUpdateClauseValue());
      }

    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
  }

  @Test
  public void ContinuationTest() {
    String continuedPragma = "claw loop-fusion   claw collapse(2)";
    analyzeValidClawLoopFusion(continuedPragma, null, true, 2, null);

    String continuedPragma2 = "claw loop-fusion   claw collapse(2) target(cpu)";
    analyzeValidClawLoopFusion(continuedPragma2, null, true, 2,
        Collections.singletonList(Target.CPU));
  }

}

