/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language.language;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.xcodeml.abstraction.ReshapeInfo;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.*;
import claw.wani.x2t.configuration.Configuration;
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
public class ClawPragmaTest {

  /**
   * Test various input for the CLAW loop fusion directive.
   */
  @Test
  public void fusionTest() {
    // Valid directives
    analyzeValidClawLoopFusion("claw loop-fusion", null, false, 0, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1)", "g1", false, 0,
        null, null);
    analyzeValidClawLoopFusion("claw loop-fusion group( g1 )", "g1", false, 0,
        null, null);
    analyzeValidClawLoopFusion("claw loop-fusion group ( g1   ) ", "g1",
        false, 0, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) collapse(2)", "g1",
        true, 2, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) collapse(3)", "g1",
        true, 3, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(3) group(g1)", "g1",
        true, 3, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(2)", null,
        true, 2, null, null);
    analyzeValidClawLoopFusion("claw loop-fusion collapse(3)", null,
        true, 3, null, null);

    // With target clause
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu)", null, false, 0,
        Collections.singletonList(Target.CPU), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(gpu)", null, false, 0,
        Collections.singletonList(Target.GPU), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(mic)", null, false, 0,
        Collections.singletonList(Target.MIC), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,cpu)", null, false,
        0, Collections.singletonList(Target.CPU), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu)", null, false,
        0, Arrays.asList(Target.CPU, Target.GPU), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu,mic)", null,
        false, 0, Arrays.asList(Target.CPU, Target.GPU, Target.MIC), null);
    analyzeValidClawLoopFusion("claw loop-fusion target(cpu,gpu) collapse(3) " +
            "group(g1)", "g1", true, 3, Arrays.asList(Target.CPU, Target.GPU),
        null);

    // Constraint clause
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) constraint(direct)",
        "g1", false, 0, null, ClawConstraint.DIRECT);
    analyzeValidClawLoopFusion("claw loop-fusion group(g1) constraint(none)",
        "g1", false, 0, null, ClawConstraint.NONE);
    analyzeValidClawLoopFusion("claw loop-fusion constraint(direct)",
        null, false, 0, null, ClawConstraint.DIRECT);
    analyzeValidClawLoopFusion("claw loop-fusion constraint(none)",
        null, false, 0, null, ClawConstraint.NONE);

    // Invalid directives
    analyzeInvalidClawLanguage("claw loop-fusiongroup(g1)");
    analyzeInvalidClawLanguage("claw loop-fusion group");
    analyzeInvalidClawLanguage("claw loop-fusion (i,j,k)");
    analyzeInvalidClawLanguage("claw loop-fusion group()");
    analyzeInvalidClawLanguage("claw loop-fusion group(   )");
  }

  /**
   * Assert the result for valid loop fusion CLAW directive
   *
   * @param raw       Raw string value of the CLAW directive to be analyzed.
   * @param groupName Group name to be found if any.
   */
  private void analyzeValidClawLoopFusion(String raw, String groupName,
                                          boolean collapse, int n,
                                          List<Target> targets,
                                          ClawConstraint constraint)
  {
    ClawPragma l = analyze(raw, ClawDirective.LOOP_FUSION);
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
    if(constraint != null) {
      assertTrue(l.hasConstraintClause());
      assertEquals(constraint, l.getConstraintClauseValue());
    } else {
      assertFalse(l.hasConstraintClause());
    }
    assertTargets(l, targets);
  }

  /**
   * Assert any invalid claw raw input
   *
   * @param raw Raw string value of the CLAW directive to be analyzed.
   */
  private void analyzeInvalidClawLanguage(String raw) {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.get().init(CompilerDirective.OPENACC, Target.GPU, null, 80);
      ClawPragma.analyze(p);
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
  public void interchangeTest() {
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

    // Invalid directives
    analyzeInvalidClawLanguage("claw loop-interchange ()");
    analyzeInvalidClawLanguage("claw loop-interchange (i,j,k) group");
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
    ClawPragma l = analyze(raw, ClawDirective.LOOP_INTERCHANGE);
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
  }

  /**
   * Assert the information sorted in the target list.
   *
   * @param l       The current ClawPragma object.
   * @param targets List of expected targets.
   */
  private void assertTargets(ClawPragma l, List<Target> targets) {
    if(targets != null && targets.size() > 0) {
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
  public void removeTest() {
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

    // Invalid directives
    analyzeInvalidClawLanguage("claw");
    analyzeInvalidClawLanguage("claw dummy");
    analyzeInvalidClawLanguage("claw end re move");
  }

  /**
   * Test various input for the CLAW model-data directive.
   */
  @Test
  public void scaModelDataTest() {
    analyzeValidSimpleClaw("claw model-data", ClawDirective.MODEL_DATA, false,
        Collections.<Target>emptyList());
    analyzeValidSimpleClaw("claw end model-data", ClawDirective.MODEL_DATA,
        true, Collections.<Target>emptyList());
    assertModelDataDirective("claw model-data layout(radiation)", "radiation");
    assertModelDataDirective("claw model-data layout( default )", "default");
  }

  /**
   * Execute the parsing on the raw string and return a ClawPragma object if
   * successful.
   *
   * @param raw       Raw directive.
   * @param directive Expected directive.
   * @return ClawPragma object with information extracted from parsing.
   */
  private ClawPragma analyze(String raw, ClawDirective directive) {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.get().init(CompilerDirective.OPENACC, Target.GPU, null, 80);
      ClawPragma l = ClawPragma.analyze(p);
      assertEquals(directive, l.getDirective());
      return l;
    } catch(IllegalDirectiveException idex) {
      fail();
    }
    return null;
  }

  /**
   * Assert the result for model-data directive
   *
   * @param raw      Raw directive.
   * @param layoutId Optional layout id to check for.
   */
  private void assertModelDataDirective(String raw, String layoutId) {
    ClawPragma l = analyze(raw, ClawDirective.MODEL_DATA);
    assertNotNull(l);
    if(layoutId == null) {
      assertFalse(l.hasLayoutClause());
      assertNull(l.getLayoutValue());
    } else {
      assertTrue(l.hasLayoutClause());
      assertEquals(layoutId, l.getLayoutValue());
    }
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
    ClawPragma l = analyze(raw, directive);
    if(isEnd) {
      assertTrue(l.isEndPragma());
    } else {
      assertFalse(l.isEndPragma());
    }
    assertTargets(l, targets);
  }

  /**
   * Test various input for the CLAW loop extract directive.
   */
  @Test
  public void extractTest() {
    // Valid directives
    ClawPragma l = analyzeValidClawLoopExtract(
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
            "map(pduco2,pduo3,palogp,palogt,podsc,podsf,podac," +
            "podaf:j1,ki3sc/j3) " +
            "map(pbsff,pbsfc:j1,ki3sc/j3) " +
            "map(pa1c,pa1f,pa2c,pa2f,pa3c,pa3f:j1) " +
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

    // Invalid directives
    analyzeInvalidClawLanguage("claw loop-extract");
    analyzeInvalidClawLanguage("claw loop   -   extract ");
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
  private ClawPragma analyzeValidClawLoopExtract(String raw, String induction,
                                                 String lower, String upper,
                                                 String step,
                                                 List<Target> targets)
  {
    ClawPragma l = analyze(raw, ClawDirective.LOOP_EXTRACT);
    assertEquals(induction, l.getRange().getInductionVar());
    assertEquals(lower, l.getRange().getLowerBound());
    assertEquals(upper, l.getRange().getUpperBound());
    if(step != null) {
      assertEquals(step, l.getRange().getStep());
    }
    assertTargets(l, targets);
    return l;
  }

  /**
   * Test various input for the CLAW kcache directive.
   */
  @Test
  public void kcacheTest() {

    // data clause + offsets
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,1)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, 1), false, false, null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(0,-1,0)",
        Arrays.asList("var1", "var2"), Arrays.asList(0, -1, 0), false, false,
        null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0)",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), false, false,
        null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0) private",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), false, true,
        null);
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
        Arrays.asList("var1", "var2"), Arrays.asList(0, -1, 0), true, false,
        null);
    analyzeValidKcache("claw kcache data(var1,var2) offset(+1,-1,0) init",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), true, false,
        null);
    analyzeValidKcache(
        "claw kcache data(var1,var2) offset(+1,-1,0) init private",
        Arrays.asList("var1", "var2"), Arrays.asList(1, -1, 0), true, true,
        null);
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

    // Invalid directives
    analyzeInvalidClawLanguage("claw k cache ");
    analyzeInvalidClawLanguage("claw k-cache");
  }

  /**
   * Assert the result for valid CLAW kcache directive
   *
   * @param raw     Raw string value of the CLAW directive to be analyzed.
   * @param data    List of identifiers to be checked.
   * @param offsets List of offsets to be checked.
   * @param init    If true, check that ClawPragma object has init clause
   *                enabled.
   */
  private void analyzeValidKcache(String raw, List<String> data,
                                  List<Integer> offsets, boolean init,
                                  boolean hasPrivate, List<Target> targets)
  {
    ClawPragma l = analyze(raw, ClawDirective.KCACHE);
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
  }

  /**
   * Test various input for the CLAW array-transform directive.
   */
  @Test
  public void arrayTransformTest() {
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
    analyzeInvalidClawLanguage("claw array-transform induction()");
    analyzeInvalidClawLanguage("claw array-transform induction");

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
    ClawPragma l = analyze(raw, ClawDirective.ARRAY_TRANSFORM);
    assertNotNull(l);
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
  }

  /**
   * Test various input for the CLAW loop-hoist directive.
   */
  @Test
  public void loopHoistTest() {
    // Valid directives
    analyzeValidLoopHoist("claw loop-hoist(i,j)",
        Arrays.asList("i", "j"), false, null, false, null, null, false, null, 0,
        false, null);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange",
        Arrays.asList("i", "j"), true, null, false, null, null, false, null, 0,
        false, null);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange(j,i)",
        Arrays.asList("i", "j"), true, Arrays.asList("j", "i"), false, null,
        null, false, null, 0, false, null);

    List<Integer> empty = Collections.emptyList();
    ReshapeInfo info1 = new ReshapeInfo("zmd", 0, empty);
    ReshapeInfo info2 =
        new ReshapeInfo("zsediflux", 1, Collections.singletonList(2));
    analyzeValidLoopHoist("claw loop-hoist(i,j) " +
            "reshape(zmd(0), zsediflux(1,2))",
        Arrays.asList("i", "j"), false, null, true,
        Arrays.asList(info1, info2), null, false, null, 0, false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) target(cpu) interchange",
        Arrays.asList("i", "j"), true, null, false, null,
        Collections.singletonList(Target.CPU), false, null, 0, false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange target(cpu, gpu)",
        Arrays.asList("i", "j"), true, null, false, null,
        Arrays.asList(Target.CPU, Target.GPU), false, null, 0, false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) target(mic)",
        Arrays.asList("i", "j"), false, null, false, null,
        Collections.singletonList(Target.MIC), false, null, 0, false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion",
        Arrays.asList("i", "j"), false, null, false, null, null, true, null, 0,
        false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion group(j1)",
        Arrays.asList("i", "j"), false, null, false, null, null, true, "j1", 0,
        false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion collapse(2)",
        Arrays.asList("i", "j"), false, null, false, null, null, true, null, 2,
        false, null);

    analyzeValidLoopHoist("claw loop-hoist(i,j) cleanup",
        Arrays.asList("i", "j"), false, null, false, null, null, false, null, 0,
        true, CompilerDirective.NONE);

    analyzeValidLoopHoist("claw loop-hoist(i,j) cleanup(acc)",
        Arrays.asList("i", "j"), false, null, false, null, null, false, null, 0,
        true, CompilerDirective.OPENACC);

    analyzeValidLoopHoist("claw loop-hoist(i,j) cleanup(omp)",
        Arrays.asList("i", "j"), false, null, false, null, null, false, null, 0,
        true, CompilerDirective.OPENMP);

    // Invalid directives
    analyzeInvalidClawLanguage("claw loop-hoist");
    analyzeInvalidClawLanguage("claw loop-hoist()");
    analyzeInvalidClawLanguage("claw loop-hoist(i,j) interchange()");
    analyzeInvalidClawLanguage("claw loop-hoist(i,j) cleanup()");
    analyzeInvalidClawLanguage("claw loop-hoist(i,j) cleanup(claw)");
    analyzeInvalidClawLanguage("claw loop-hoist(i,j) cleanup(dummy)");

    analyzeValidSimpleClaw("claw end loop-hoist",
        ClawDirective.LOOP_HOIST, true, null);
    analyzeValidSimpleClaw("claw   end   loop-hoist  ",
        ClawDirective.LOOP_HOIST, true, null);
  }

  /**
   * Assert the result for valid CLAW loop-hoist directive
   *
   * @param raw         Raw string value of the CLAW directive to be analyzed.
   * @param inductions  List of induction variables to be checked.
   * @param interchange If true, the interchange clause is set.
   * @param indexes     Interchange indexes values.
   * @param reshape     If true, the reshape clause is set.
   * @param infos       Reshape clause values.
   * @param targets     Target clause values. Null if not set.
   * @param fusion      If true, the fusion clause is set.
   * @param group       Group clause value. Null if not set.
   * @param collapse    Collapse clause value. 0 if not set.
   */
  private void analyzeValidLoopHoist(String raw, List<String> inductions,
                                     boolean interchange, List<String> indexes,
                                     boolean reshape,
                                     List<ReshapeInfo> infos,
                                     List<Target> targets, boolean fusion,
                                     String group, int collapse,
                                     boolean cleanup,
                                     CompilerDirective cleanupValue)
  {
    ClawPragma l = analyze(raw, ClawDirective.LOOP_HOIST);
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

    assertEquals(fusion, l.hasFusionClause());

    if(group != null) {
      assertTrue(l.hasGroupClause());
      assertEquals(group, l.getGroupValue());
    } else {
      assertFalse(l.hasGroupClause());
    }

    if(collapse > 0) {
      assertTrue(l.hasCollapseClause());
      assertEquals(collapse, l.getCollapseValue());
    } else {
      assertFalse(l.hasCollapseClause());
    }

    if(cleanup) {
      assertTrue(l.hasCleanupClause());
      assertSame(cleanupValue, l.getCleanupClauseValue());
    } else {
      assertFalse(l.hasCleanupClause());
    }
  }

  /**
   * Test various input for the CLAW call directive.
   */
  @Test
  public void arrayToFctCallTest() {
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

    // Invalid directives
    analyzeInvalidClawLanguage("claw call ");
    analyzeInvalidClawLanguage("claw call v=");
    analyzeInvalidClawLanguage("claw call v=()");
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
    ClawPragma l = analyze(raw, ClawDirective.ARRAY_TO_CALL);
    assertEquals(params.size(), l.getFctParams().size());
    for(int i = 0; i < params.size(); ++i) {
      assertEquals(params.get(i), l.getFctParams().get(i));
    }

    assertEquals(arrayName, l.getArrayName());
    assertEquals(fctName, l.getFctName());
    assertTargets(l, targets);
  }

  /**
   * Test !$claw nodep directive
   */
  @Test
  public void nodepTest() {
    ClawPragma l = analyze("claw nodep", ClawDirective.NO_DEP);
  }

  /**
   * Test various input for the CLAW sca directive.
   */
  @Test
  public void scaOverClauseTest() {
    DimensionDefinition d1 = new DimensionDefinition("i", "1", "ni");
    DimensionDefinition d2 = new DimensionDefinition("j", "1", "nj");
    List<String> data1 = Arrays.asList("a", "b", "c");
    List<List<String>> data1Lst = Collections.singletonList(data1);

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    analyzeValidOverSCA("claw define dimension i(1:ni) sca " +
            "data(a,b,c) over (i,:)", data1Lst,
        Collections.singletonList(Collections.singletonList(d1)));

    d1.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverSCA("claw define dimension i(1:ni) sca " +
            "data(a,b,c) over (:,i)", data1Lst,
        Collections.singletonList(Collections.singletonList(d1)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.BEFORE);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (i,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.AFTER);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (:,i,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    d2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (:,i,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (i,:,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (i,:,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b,c) over (:,i,:,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverSCA("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) sca " +
            "data(a,b) over (i,:)" +
            "data(c) over (:,j)",
        Arrays.asList(Arrays.asList("a", "b"), Collections.singletonList("c")),
        Arrays.asList(Collections.singletonList(d1),
            Collections.singletonList(d2)));
  }

  /**
   * Assert the result for valid CLAW sca directive with data over
   * clause.
   *
   * @param raw        Raw string value of the CLAW directive to be analyzed.
   * @param datas      Reference list for the data clause values.
   * @param dimensions Reference list of dimensions.
   */
  private void analyzeValidOverSCA(String raw, List<List<String>> datas,
                                   List<List<DimensionDefinition>> dimensions)
  {
    ClawPragma l = analyze(raw, ClawDirective.SCA);
    if(datas != null) {
      assertEquals(datas.size(), dimensions.size());
      assertTrue(l.hasDataOverClause());

      for(int j = 0; j < datas.size(); ++j) {
        List<String> data = datas.get(j);
        List<DimensionDefinition> dimension = dimensions.get(j);

        for(String id : data) {
          assertNotNull(l.getLayoutForData(id));
          List<DimensionDefinition> dims = l.getLayoutForData(id);
          assertEquals(dimension.size(), dims.size());
          for(int i = 0; i < dimension.size(); ++i) {
            assertEquals(dimension.get(i).getIdentifier(),
                dims.get(i).getIdentifier());
            assertEquals(dimension.get(i).getInsertionPosition(),
                dims.get(i).getInsertionPosition());

            assertEquals(dimension.get(i).getLowerBound().isVar(),
                dims.get(i).getLowerBound().isVar());
            assertEquals(dimension.get(i).getLowerBound().getValue(),
                dims.get(i).getLowerBound().getValue());
            assertEquals(dimension.get(i).getLowerBound().getIntValue(),
                dims.get(i).getLowerBound().getIntValue());

            assertEquals(dimension.get(i).getUpperBound().isVar(),
                dims.get(i).getUpperBound().isVar());
            assertEquals(dimension.get(i).getUpperBound().getValue(),
                dims.get(i).getUpperBound().getValue());
            assertEquals(dimension.get(i).getUpperBound().getIntValue(),
                dims.get(i).getUpperBound().getIntValue());
          }
        }
      }

    }
  }

  private DimensionDefinition getDimDef(String id, String start, String end,
                                        InsertionPosition pos)
  {
    DimensionDefinition dimDef = new DimensionDefinition(id, start, end);
    dimDef.setInsertionPosition(pos);
    return dimDef;
  }

  /**
   * Test various input for the CLAW SCA directive.
   */
  @Test
  public void scaTest() {

    // Valid directives
    DimensionDefinition d1 = new DimensionDefinition("i", "1", "nx");
    List<String> data1 = Arrays.asList("t", "qc", "qv");

    DimensionDefinition dimI =
        getDimDef("i", "1", "nx", InsertionPosition.BEFORE);
    List<String> layout1 = Arrays.asList("i", ":");

    List<String> ijc = Arrays.asList("i", "j", ":");
    List<String> icj = Arrays.asList("i", ":", "j");
    List<String> cij = Arrays.asList(":", "i", "j");

    List<List<String>> layouts1 = Arrays.asList(layout1, layout1, layout1);
    List<List<String>> over2 = Collections.singletonList(icj);
    List<List<String>> over3 = Collections.singletonList(cij);

    analyzeValidSCA("claw sca", null, null, null, null, null, true);

    analyzeValidSCA("claw define dimension i(1:nx)" +
            " sca data(t,qc,qv) over (i,:)", data1,
        Collections.singletonList(d1), null, null, null, false);

    DimensionDefinition d2 = new DimensionDefinition("j", "1", "ny");
    analyzeValidSCA("claw define dimension j(1:ny)" +
            "sca data(t,qc,qv) over (j,:)", data1,
        Collections.singletonList(d2), null, null, null, false);

    DimensionDefinition d3 = new DimensionDefinition("j", "1", "10");
    analyzeValidSCA("claw define dimension j(1:10) " +
            "sca data(t,qc,qv) over (j,:)", data1,
        Collections.singletonList(d3), null, null, null, false);

    DimensionDefinition d4 = new DimensionDefinition("j", "jstart", "10");
    analyzeValidSCA("claw define dimension j(jstart:10) " +
            "sca data(t,qc,qv) over (j,:)", data1,
        Collections.singletonList(d4), null, null, null, false);

    DimensionDefinition d5 = new DimensionDefinition("j", "jstart", "ny");
    analyzeValidSCA("claw define dimension j(jstart:ny) " +
            "sca data(t,qc,qv) over (j,:)", data1,
        Collections.singletonList(d5), null, null, null, false);

    DimensionDefinition d6 = new DimensionDefinition("j", "jstart", "ny");
    analyzeValidSCA("claw define dimension j(jstart:ny) sca", null,
        Collections.singletonList(d6), null, null, null, false);

    analyzeValidSCA("claw define dimension i(1:nx) sca scalar(s1,s2)", null,
        Collections.singletonList(d1), null, null,
        Arrays.asList("s1", "s2"), false);

    analyzeValidSCA("claw sca forward",
        null, null, null, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t,qc,qv) over (i,j,:)",
        data1, Arrays.asList(d1, d2), null, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t,qc,qv) over (:,i,j)",
        data1, Arrays.asList(d1, d2), null, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t,qc,qv) over (i,:,j)",
        data1, Arrays.asList(d1, d2), null, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j)",
        data1, Arrays.asList(d1, d2), null, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "copy", data1, Arrays.asList(d1, d2),
        DataMovement.BOTH, null, null, false);
    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "copy(in)", data1, Arrays.asList(d1, d2),
        DataMovement.DEVICE, null, null, false);
    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "copy(out)", data1, Arrays.asList(d1, d2),
        DataMovement.HOST, null, null, false);

    DimensionDefinition d7 = new DimensionDefinition("c", "1", "nc");
    analyzeValidSCA("claw define dimension c(1:nc) sca copy",
        null, Collections.singletonList(d7),
        DataMovement.BOTH, null, null, false);
    analyzeValidSCA("claw define dimension c(1:nc) " +
            "sca copy(in)", null, Collections.singletonList(d7),
        DataMovement.DEVICE, null, null, false);
    analyzeValidSCA("claw define dimension c(1:nc) " +
            "sca copy(out)", null, Collections.singletonList(d7),
        DataMovement.HOST, null, null, false);

    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "update", data1, Arrays.asList(d1, d2),
        null, DataMovement.BOTH, null, false);
    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "update(in)", data1, Arrays.asList(d1, d2),
        null, DataMovement.DEVICE, null, false);
    analyzeValidSCA("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "sca data(t , qc , qv) over (i,:,j) " +
            "update(out)", data1, Arrays.asList(d1, d2),
        null, DataMovement.HOST, null, false);

    analyzeValidSCA("claw define dimension c(1:nc) sca update",
        null, Collections.singletonList(d7), null,
        DataMovement.BOTH, null, false);
    analyzeValidSCA("claw define dimension c(1:nc) " +
            "sca update(in)", null, Collections.singletonList(d7),
        null, DataMovement.DEVICE, null, false);
    analyzeValidSCA("claw define dimension c(1:nc) " +
            "sca update(out)", null, Collections.singletonList(d7),
        null, DataMovement.HOST, null, false);

    analyzeValidSCA("claw sca forward copy",
        null, null, DataMovement.BOTH, null, null, false);
    analyzeValidSCA("claw sca forward copy(in)",
        null, null, DataMovement.DEVICE, null, null, false);
    analyzeValidSCA("claw sca forward copy(out)",
        null, null, DataMovement.HOST, null, null, false);

    analyzeValidSCA("claw sca forward update",
        null, null, null, DataMovement.BOTH, null, false);
    analyzeValidSCA("claw sca forward update(in)",
        null, null, null, DataMovement.DEVICE, null, false);
    analyzeValidSCA("claw sca forward update(out)",
        null, null, null, DataMovement.HOST, null, false);

    List<String> dataLst2 = Arrays.asList("t", "q");

    List<String> ic = Arrays.asList("i", ":");
    List<String> ci = Arrays.asList(":", "i");
    List<List<String>> over4 = Arrays.asList(ic, ci);

    analyzeValidSCA("claw  define dimension i(1:nx) " +
            "sca data(t) over (i,:) data(q) over(:,i)", dataLst2,
        Collections.singletonList(d1), null, null, null, false);

    // Invalid directives
    analyzeInvalidClawLanguage("claw sca data over ");
    analyzeInvalidClawLanguage("claw sca data");
    analyzeInvalidClawLanguage("claw sca over");
    analyzeInvalidClawLanguage("claw parallelite data() over ()");
  }

  @Test
  public void scaDataMgtTest() {
    analyzeValidScaDataMgtString("claw sca forward create",
        null, null, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "update", DataMovement.BOTH, null, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "update(in)", DataMovement.DEVICE, null, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "update(out)", DataMovement.HOST, null, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "copy", null, DataMovement.BOTH, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "copy(in)", null, DataMovement.DEVICE, true);
    analyzeValidScaDataMgtString("claw sca forward create " +
        "copy(out)", null, DataMovement.HOST, true);

    analyzeValidScaDataMgtString("claw sca forward update",
        DataMovement.BOTH, null, false);
    analyzeValidScaDataMgtString("claw sca forward update(in)",
        DataMovement.DEVICE, null, false);
    analyzeValidScaDataMgtString("claw sca forward update(out)",
        DataMovement.HOST, null, false);
    analyzeValidScaDataMgtString("claw sca forward copy", null,
        DataMovement.BOTH, false);
    analyzeValidScaDataMgtString("claw sca forward copy(in)",
        null, DataMovement.DEVICE, false);
    analyzeValidScaDataMgtString("claw sca forward copy(out)",
        null, DataMovement.HOST, false);
  }

  /**
   * Assert the result for valid CLAW SCA directive
   */
  private void analyzeValidScaDataMgtString(String raw, DataMovement update,
                                            DataMovement copy,
                                            boolean createClause)
  {
    ClawPragma l = analyze(raw, ClawDirective.SCA);
    assertEquals(createClause, l.hasCreateClause());
    if(update != null) {
      assertTrue(l.hasUpdateClause());
      assertEquals(update, l.getUpdateClauseValue());
    } else {
      assertFalse(l.hasUpdateClause());
    }
    if(copy != null) {
      assertTrue(l.hasCopyClause());
      assertEquals(copy, l.getCopyClauseValue());
    } else {
      assertFalse(l.hasCopyClause());
    }
  }

  /**
   * Assert the result for valid CLAW SCA directive
   *
   * @param raw          Raw string value of the CLAW directive to be analyzed.
   * @param data         Reference list for the data clause values.
   * @param dimensions   Reference list of dimensions.
   * @param copyClause   Expected value for copy clause (Null if no copy clause)
   * @param updateClause Expected value for update clause
   *                     (Null if no update clause)
   */
  private void analyzeValidSCA(String raw, List<String> data,
                               List<DimensionDefinition> dimensions,
                               DataMovement copyClause,
                               DataMovement updateClause,
                               List<String> scalarData, boolean isModelConfig)
  {
    ClawPragma l = analyze(raw, ClawDirective.SCA);

    assertEquals(0, l.getErrors().size());

    if(data != null) {
      assertTrue(l.hasDataOverClause());
      assertEquals(data.size(), l.getDataOverClauseValues().size());
      for(int i = 0; i < data.size(); ++i) {
        assertTrue(l.getDataOverClauseValues().contains(data.get(i)));
        assertTrue(l.getLocalModelConfig().hasLayout(data.get(i)));
      }
    }

    if(dimensions != null) {
      assertEquals(dimensions.size(),
          l.getLocalModelConfig().getNbDimensions());

      for(DimensionDefinition expected : dimensions) {
        DimensionDefinition actual =
            l.getLocalModelConfig().getDimension(expected.getIdentifier());
        assertNotNull(actual);
        assertEquals(expected.getIdentifier(), actual.getIdentifier());
        assertEquals(expected.getLowerBound().isVar(),
            actual.getLowerBound().isVar());
        assertEquals(expected.getUpperBound().isVar(),
            actual.getUpperBound().isVar());
        assertEquals(expected.getLowerBound().getIntValue(),
            actual.getLowerBound().getIntValue());
        assertEquals(expected.getUpperBound().getIntValue(),
            actual.getUpperBound().getIntValue());
        assertEquals(expected.getLowerBound().getValue(),
            actual.getLowerBound().getValue());
        assertEquals(expected.getUpperBound().getValue(),
            actual.getUpperBound().getValue());
      }
    }

    if(scalarData != null) {
      assertTrue(l.hasScalarClause());
      assertEquals(scalarData.size(), l.getScalarClauseValues().size());
      for(int i = 0; i < scalarData.size(); ++i) {
        assertEquals(scalarData.get(i), l.getScalarClauseValues().get(i));
      }
    }

    if(data == null && dimensions == null && !isModelConfig) {
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
  }

  @Test
  public void continuationTest() {
    String continuedPragma = "claw loop-fusion   claw collapse(2)";
    analyzeValidClawLoopFusion(continuedPragma, null, true, 2, null, null);

    String continuedPragma2 = "claw loop-fusion   claw collapse(2) target(cpu)";
    analyzeValidClawLoopFusion(continuedPragma2, null, true, 2,
        Collections.singletonList(Target.CPU), null);
  }

  @Test
  public void errorHandlingTest() {
    analyzeErrors("claw loop-fusion group(g", 1);
    analyzeErrors("claw loop-fusion group", 1);
    analyzeErrors("claw loop", 19);
  }

  private void analyzeErrors(String pragma, int nbExpectedToken) {
    Xnode p = XmlHelper.createXpragma();
    p.setValue(pragma);
    p.setLine(1);
    Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
    Context.get().init(CompilerDirective.OPENACC, Target.GPU, null, 80);
    try {
      ClawPragma.analyze(p);
    } catch(IllegalDirectiveException e) {
      if(nbExpectedToken != 0) {
        assertEquals(nbExpectedToken, e.getExpectedTokens().size());
      }
      assertNotNull(e.getMessage());
    }
  }

  @Test
  public void primitiveTest() {
    analyzeValidSimpleClaw("claw omp do", ClawDirective.PRIMITIVE, false, null);
    analyzeValidSimpleClaw("claw   omp end do", ClawDirective.PRIMITIVE, false,
        null);
    analyzeValidSimpleClaw("claw acc parallel", ClawDirective.PRIMITIVE, false,
        null);
    analyzeValidSimpleClaw("claw acc end parallel", ClawDirective.PRIMITIVE,
        false, null);
  }

  @Test
  public void verbatimTest() {
    analyzeValidSimpleClaw("claw verbatim if (test) then",
        ClawDirective.VERBATIM, false, null);
    analyzeValidSimpleClaw("claw verbatim end if",
        ClawDirective.VERBATIM, false, null);
    analyzeValidSimpleClaw("claw verbatim print*,'test'",
        ClawDirective.VERBATIM, false, null);
  }

  @Test
  public void ifExtractTest() {
    analyzeValidSimpleClaw("claw if-extract", ClawDirective.IF_EXTRACT,
        false, null);
  }
}
