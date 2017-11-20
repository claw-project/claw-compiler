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
        "group(g1)", "g1", true, 3, Arrays.asList(Target.CPU, Target.GPU), null);

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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
      if(constraint != null) {
        assertTrue(l.hasConstraintClause());
        assertEquals(constraint, l.getConstraintClauseValue());
      } else {
        assertFalse(l.hasConstraintClause());
      }

      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      fail();
    }
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
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
   * @param l       The current ClawPragma object.
   * @param targets List of expected targets.
   */
  private void assertTargets(ClawPragma l, List<Target> targets) {
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
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
      assertEquals(directive, l.getDirective());
      if(isEnd) {
        assertTrue(l.isEndPragma());
      } else {
        assertFalse(l.isEndPragma());
      }
      assertTargets(l, targets);
    } catch(IllegalDirectiveException idex) {
      System.err.println(idex.getMessage());
      fail();
    }
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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
  public void kcacheTest() {

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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
  public void loopHoistTest() {
    // Valid directives
    analyzeValidLoopHoist("claw loop-hoist(i,j)",
        Arrays.asList("i", "j"), false, null, false, null, null, false, null, 0);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange",
        Arrays.asList("i", "j"), true, null, false, null, null, false, null, 0);
    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange(j,i)",
        Arrays.asList("i", "j"), true, Arrays.asList("j", "i"), false, null,
        null, false, null, 0);

    List<Integer> empty = Collections.emptyList();
    ReshapeInfo info1 = new ReshapeInfo("zmd", 0, empty);
    ReshapeInfo info2 =
        new ReshapeInfo("zsediflux", 1, Collections.singletonList(2));
    analyzeValidLoopHoist("claw loop-hoist(i,j) reshape(zmd(0), zsediflux(1,2))",
        Arrays.asList("i", "j"), false, null, true,
        Arrays.asList(info1, info2), null, false, null, 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) target(cpu) interchange",
        Arrays.asList("i", "j"), true, null, false, null,
        Collections.singletonList(Target.CPU), false, null, 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) interchange target(cpu, gpu)",
        Arrays.asList("i", "j"), true, null, false, null,
        Arrays.asList(Target.CPU, Target.GPU), false, null, 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) target(mic)",
        Arrays.asList("i", "j"), false, null, false, null,
        Collections.singletonList(Target.MIC), false, null, 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion",
        Arrays.asList("i", "j"), false, null, false, null, null, true, null, 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion group(j1)",
        Arrays.asList("i", "j"), false, null, false, null, null, true, "j1", 0);

    analyzeValidLoopHoist("claw loop-hoist(i,j) fusion collapse(2)",
        Arrays.asList("i", "j"), false, null, false, null, null, true, null, 2);

    // Invalid directives
    analyzeInvalidClawLanguage("claw loop-hoist");
    analyzeInvalidClawLanguage("claw loop-hoist()");
    analyzeInvalidClawLanguage("claw loop-hoist(i,j) interchange()");

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
                                     String group, int collapse)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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

    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
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
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
   * Test !$claw nodep directive
   */
  @Test
  public void nodepTest() {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue("claw nodep");
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
      assertEquals(ClawDirective.NO_DEP, l.getDirective());
    } catch(Exception e) {
      fail();
    }
  }

  /**
   * Test various input for the CLAW one_column directive.
   */
  @Test
  public void parallelizeOverClauseTest() {
    DimensionDefinition d1 = new DimensionDefinition("i", "1", "ni");
    DimensionDefinition d2 = new DimensionDefinition("j", "1", "nj");
    List<String> data1 = Arrays.asList("a", "b", "c");
    List<List<String>> data1Lst = Collections.singletonList(data1);

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "parallelize " +
            "data(a,b,c) over (i,:)", data1Lst,
        Collections.singletonList(Collections.singletonList(d1)));

    d1.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "parallelize " +
            "data(a,b,c) over (:,i)", data1Lst,
        Collections.singletonList(Collections.singletonList(d1)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.BEFORE);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (i,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.AFTER);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (:,i,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    d2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (:,i,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (i,:,j,:)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (i,:,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.IN_MIDDLE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b,c) over (:,i,:,j)", data1Lst,
        Collections.singletonList(Arrays.asList(d1, d2)));

    d1.setInsertionPosition(InsertionPosition.BEFORE);
    d2.setInsertionPosition(InsertionPosition.AFTER);
    analyzeValidOverParallelize("claw define dimension i(1:ni) " +
            "define dimension j(1:nj) " +
            "parallelize " +
            "data(a,b) over (i,:)" +
            "data(c) over (:,j)",
        Arrays.asList(Arrays.asList("a", "b"), Collections.singletonList("c")),
        Arrays.asList(Collections.singletonList(d1),
            Collections.singletonList(d2)));
  }

  /**
   * Assert the result for valid CLAW parallelize directive with data over
   * clause.
   *
   * @param raw        Raw string value of the CLAW directive to be analyzed.
   * @param datas      Reference list for the data clause values.
   * @param dimensions Reference list of dimensions.
   */
  private void analyzeValidOverParallelize(String raw,
                                           List<List<String>> datas,
                                           List<List<DimensionDefinition>>
                                               dimensions)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
      assertEquals(ClawDirective.PARALLELIZE, l.getDirective());

      if(datas != null) {
        assertEquals(datas.size(), dimensions.size());
        assertTrue(l.hasOverDataClause());

        for(int j = 0; j < datas.size(); ++j) {
          List<String> data = datas.get(j);
          List<DimensionDefinition> dimension = dimensions.get(j);

          for(String id : data) {
            assertNotNull(l.getDimensionsForData(id));
            List<DimensionDefinition> dims = l.getDimensionsForData(id);
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
    DimensionDefinition d1 = new DimensionDefinition("i", "1", "nx");
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
        dataLst1, over1, Collections.singletonList(d1), null, null, null);

    DimensionDefinition d2 = new DimensionDefinition("j", "1", "ny");
    analyzeValidParallelize("claw define dimension j(1:ny)" +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d2), null, null, null);

    DimensionDefinition d3 = new DimensionDefinition("j", "1", "10");
    analyzeValidParallelize("claw define dimension j(1:10) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d3), null, null, null);

    DimensionDefinition d4 = new DimensionDefinition("j", "jstart", "10");
    analyzeValidParallelize("claw define dimension j(jstart:10) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d4), null, null, null);

    DimensionDefinition d5 = new DimensionDefinition("j", "jstart", "ny");
    analyzeValidParallelize("claw define dimension j(jstart:ny) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Collections.singletonList(d5), null, null, null);

    DimensionDefinition d6 = new DimensionDefinition("j", "jstart", "ny");
    analyzeValidParallelize("claw define dimension j(jstart:ny) parallelize",
        null, null, Collections.singletonList(d6), null, null, null);

    analyzeValidParallelize("claw define dimension i(1:nx)" +
            " parallelize scalar(s1,s2)",
        null, null, Collections.singletonList(d1), null, null,
        Arrays.asList("s1", "s2"));

    analyzeValidParallelize("claw parallelize forward",
        null, null, null, null, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t,qc,qv) over (i,j,:)",
        dataLst1, over1, Arrays.asList(d1, d2), null, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t,qc,qv) over (:,i,j)",
        dataLst1, over3, Arrays.asList(d1, d2), null, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t,qc,qv) over (i,:,j)",
        dataLst1, over2, Arrays.asList(d1, d2), null, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j)",
        dataLst1, over2, Arrays.asList(d1, d2), null, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "copy", dataLst1, over2, Arrays.asList(d1, d2),
        DataMovement.BOTH, null, null);
    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "copy(in)", dataLst1, over2, Arrays.asList(d1, d2),
        DataMovement.DEVICE, null, null);
    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "copy(out)", dataLst1, over2, Arrays.asList(d1, d2),
        DataMovement.HOST, null, null);

    DimensionDefinition d7 = new DimensionDefinition("c", "1", "nc");
    analyzeValidParallelize("claw define dimension c(1:nc) parallelize copy",
        null, null, Collections.singletonList(d7), DataMovement.BOTH, null, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize copy(in)", null, null, Collections.singletonList(d7),
        DataMovement.DEVICE, null, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize copy(out)", null, null, Collections.singletonList(d7),
        DataMovement.HOST, null, null);

    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "update", dataLst1, over2, Arrays.asList(d1, d2),
        null, DataMovement.BOTH, null);
    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "update(in)", dataLst1, over2, Arrays.asList(d1, d2),
        null, DataMovement.DEVICE, null);
    analyzeValidParallelize("claw " +
            "define dimension i(1:nx) " +
            "define dimension j(1:ny) " +
            "parallelize data(t , qc , qv) over (i,:,j) " +
            "update(out)", dataLst1, over2, Arrays.asList(d1, d2),
        null, DataMovement.HOST, null);

    analyzeValidParallelize("claw define dimension c(1:nc) parallelize update",
        null, null, Collections.singletonList(d7), null, DataMovement.BOTH, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize update(in)", null, null, Collections.singletonList(d7),
        null, DataMovement.DEVICE, null);
    analyzeValidParallelize("claw define dimension c(1:nc) " +
            "parallelize update(out)", null, null, Collections.singletonList(d7),
        null, DataMovement.HOST, null);

    analyzeValidParallelize("claw parallelize forward copy",
        null, null, null, DataMovement.BOTH, null, null);
    analyzeValidParallelize("claw parallelize forward copy(in)",
        null, null, null, DataMovement.DEVICE, null, null);
    analyzeValidParallelize("claw parallelize forward copy(out)",
        null, null, null, DataMovement.HOST, null, null);

    analyzeValidParallelize("claw parallelize forward update",
        null, null, null, null, DataMovement.BOTH, null);
    analyzeValidParallelize("claw parallelize forward update(in)",
        null, null, null, null, DataMovement.DEVICE, null);
    analyzeValidParallelize("claw parallelize forward update(out)",
        null, null, null, null, DataMovement.HOST, null);

    List<String> data2 = Collections.singletonList("t");
    List<String> data3 = Collections.singletonList("q");
    List<List<String>> dataLst2 = Arrays.asList(data2, data3);

    List<String> ic = Arrays.asList("i", ":");
    List<String> ci = Arrays.asList(":", "i");
    List<List<String>> over4 = Arrays.asList(ic, ci);

    analyzeValidParallelize("claw  define dimension i(1:nx) " +
            "parallelize data(t) over (i,:) data(q) over(:,i)",
        dataLst2, over4, Collections.singletonList(d1), null, null, null);

    // Invalid directives
    analyzeInvalidClawLanguage("claw parallelize data over ");
    analyzeInvalidClawLanguage("claw parallelize data");
    analyzeInvalidClawLanguage("claw parallelize over");
    analyzeInvalidClawLanguage("claw parallelite data() over ()");
  }

  @Test
  public void parallelizeDataMgtTest() {
    analyzeValidParallelizeDataMgtString("claw parallelize forward create",
        null, null, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "update", DataMovement.BOTH, null, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "update(in)", DataMovement.DEVICE, null, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "update(out)", DataMovement.HOST, null, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "copy", null, DataMovement.BOTH, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "copy(in)", null, DataMovement.DEVICE, true);
    analyzeValidParallelizeDataMgtString("claw parallelize forward create " +
        "copy(out)", null, DataMovement.HOST, true);

    analyzeValidParallelizeDataMgtString("claw parallelize forward update",
        DataMovement.BOTH, null, false);
    analyzeValidParallelizeDataMgtString("claw parallelize forward update(in)",
        DataMovement.DEVICE, null, false);
    analyzeValidParallelizeDataMgtString("claw parallelize forward update(out)",
        DataMovement.HOST, null, false);
    analyzeValidParallelizeDataMgtString("claw parallelize forward copy", null,
        DataMovement.BOTH, false);
    analyzeValidParallelizeDataMgtString("claw parallelize forward copy(in)",
        null, DataMovement.DEVICE, false);
    analyzeValidParallelizeDataMgtString("claw parallelize forward copy(out)",
        null, DataMovement.HOST, false);
  }

  /**
   * Assert the result for valid CLAW parallelize directive
   */
  private void analyzeValidParallelizeDataMgtString(String raw, DataMovement update,
                                                    DataMovement copy,
                                                    boolean createClause)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
      assertEquals(ClawDirective.PARALLELIZE, l.getDirective());

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

    } catch(IllegalDirectiveException idex) {
      System.err.print(idex.getMessage());
      fail();
    }
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
                                       List<DimensionDefinition> dimensions,
                                       DataMovement copyClause, DataMovement updateClause,
                                       List<String> scalarData)
  {
    try {
      Xnode p = XmlHelper.createXpragma();
      p.setValue(raw);
      Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
      Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
      ClawPragma l = ClawPragma.analyze(p);
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
          assertEquals(dimensions.get(i).getLowerBound().isVar(),
              l.getDimensionValues().get(i).getLowerBound().isVar());
          assertEquals(dimensions.get(i).getUpperBound().isVar(),
              l.getDimensionValues().get(i).getUpperBound().isVar());
          assertEquals(dimensions.get(i).getLowerBound().getIntValue(),
              l.getDimensionValues().get(i).getLowerBound().getIntValue());
          assertEquals(dimensions.get(i).getUpperBound().getIntValue(),
              l.getDimensionValues().get(i).getUpperBound().getIntValue());
          assertEquals(dimensions.get(i).getLowerBound().getValue(),
              l.getDimensionValues().get(i).getLowerBound().getValue());
          assertEquals(dimensions.get(i).getUpperBound().getValue(),
              l.getDimensionValues().get(i).getUpperBound().getValue());
        }
      }

      if(scalarData != null) {
        assertTrue(l.hasScalarClause());
        assertEquals(scalarData.size(), l.getScalarClauseValues().size());
        for(int i = 0; i < scalarData.size(); ++i) {
          assertEquals(scalarData.get(i), l.getScalarClauseValues().get(i));
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
    analyzeErrors("claw loop", 17);
  }

  private void analyzeErrors(String pragma, int nbExpectedToken) {
    Xnode p = XmlHelper.createXpragma();
    p.setValue(pragma);
    p.setLine(1);
    Configuration.get().init(CompilerDirective.OPENACC, Target.GPU);
    Context.init(CompilerDirective.OPENACC, Target.GPU, 80);
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
