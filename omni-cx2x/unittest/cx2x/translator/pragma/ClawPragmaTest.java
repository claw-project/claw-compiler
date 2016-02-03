/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import static org.junit.Assert.*;

import cx2x.translator.common.Constant;
import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.Xpragma;
import org.junit.Test;
import java.util.List;

/**
 * Test the features of the ClawPragma enum.
 *
 * @author clementval
 */

public class ClawPragmaTest {

  @Test
  public void isValidTest() {
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);

    // loop-fusion
    pragma1.setData("claw loop-fusion");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group(g1)");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group( g1 )");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group ( g1   ) ");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusiongroup(g1)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion (i,j,k)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group()");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-fusion group(   )");
    assertFalse(ClawPragma.isValid(pragma1));

    // loop-interchange
    pragma1.setData("claw loop-interchange");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-interchange (i,j,k)");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-interchange (  i,j,k  ) ");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-interchange ()");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-interchange (  )");
    assertFalse(ClawPragma.isValid(pragma1));

    // loop-extract
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i)");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) " +
        "map(value1,value2:i)");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) " +
        "map(value1, value2:i)");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) " +
        "map(value1, value2)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(:i)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract map(value1:i)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range() map(value1:i)");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map()");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range() map()");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw loop-extract range map");
    assertFalse(ClawPragma.isValid(pragma1));


    pragma1.setData("claw loop-extract range(j1=ki1sc,ki1ec) "
        + " map(pduh2oc,pduh2of:j1,ki3sc/j3) "
        + " map(pduco2,pduo3,palogp,palogt,podsc,podsf,podac,podaf:j1,ki3sc/j3)"
        + " map(pbsff,pbsfc:j1,ki3sc/j3) map(pa1c,pa1f,pa2c,pa2f,pa3c,pa3f:j1) "
        + " fusion group(j1)");
    assertTrue(ClawPragma.isValid(pragma1));


    // remove
    pragma1.setData("claw remove");
    assertTrue(ClawPragma.isValid(pragma1));
    pragma1.setData("claw end remove");
    assertTrue(ClawPragma.isValid(pragma1));

    // invalid dummy directives
    pragma1.setData("claw");
    assertFalse(ClawPragma.isValid(pragma1));
    pragma1.setData("claw dummy-directive");
    assertFalse(ClawPragma.isValid(pragma1));

  }

  @Test
  public void parallelOptionTest(){
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) parallel ");
    assertTrue(ClawPragma.hasParallelOption(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i)");
    assertFalse(ClawPragma.hasParallelOption(pragma1));
  }

  @Test
  public void accOptionTest(){
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) parallel acc(loop gang vector)");
    assertEquals("loop gang vector", ClawPragma.getAccOptionValue(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) parallel acc(loop seq)");
    assertEquals("loop seq", ClawPragma.getAccOptionValue(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) parallel acc()");
    assertNull(ClawPragma.getAccOptionValue(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i)");
    assertNull(ClawPragma.getAccOptionValue(pragma1));
  }

  @Test
  public void extractMappingTest(){
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-extract range(j1=ki1sc,ki1ec) "
        + " map(pduh2oc,pduh2of:j1,ki3sc/j3) "
        + " map(pduco2,pduo3,palogp,palogt,podsc,podsf,podac,podaf:j1,ki3sc/j3)"
        + " map(pbsff,pbsfc:j1,ki3sc/j3) map(pa1c,pa1f,pa2c,pa2f,pa3c,pa3f:j1) "
        + " fusion group(j1)");
    List<ClawMapping> mappings = null;
    try {
       mappings = ClawPragma.extractMappingInformation(pragma1);
    } catch (IllegalDirectiveException ide){
      fail();
    }

    assertNotNull(mappings);
    assertEquals(4, mappings.size());

    assertEquals(2, mappings.get(0).getMappedDimensions());
    assertEquals(2, mappings.get(0).getMappedVariables().size());
    assertEquals(2, mappings.get(0).getMappingVariables().size());


    assertEquals(2, mappings.get(1).getMappedDimensions());
    assertEquals(8, mappings.get(1).getMappedVariables().size());
    assertEquals(2, mappings.get(1).getMappingVariables().size());


    assertEquals(2, mappings.get(2).getMappedDimensions());
    assertEquals(2, mappings.get(2).getMappedVariables().size());
    assertEquals(2, mappings.get(2).getMappingVariables().size());


    assertEquals(1, mappings.get(3).getMappedDimensions());
    assertEquals(6, mappings.get(3).getMappedVariables().size());
    assertEquals(1, mappings.get(3).getMappingVariables().size());

  }

  @Test
  public void getGroupOptionTest(){
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-fusion group (g1)");
    ClawPragma cp1 = ClawPragma.getDirective(pragma1);
    assertNotNull(cp1);
    assertTrue(cp1.isDirective());
    assertEquals(ClawPragma.LOOP_FUSION, cp1);
    assertEquals("g1", ClawPragma.getGroupOptionValue(pragma1));
    pragma1.setData("claw loop-fusion group (g1) ");
    assertEquals("g1", ClawPragma.getGroupOptionValue(pragma1));
    pragma1.setData("claw loop-fusion group ( g1  )  ");
    assertEquals("g1", ClawPragma.getGroupOptionValue(pragma1));
    pragma1.setData("claw loop-fusion group()");
    assertNull(ClawPragma.getGroupOptionValue(pragma1));
    pragma1.setData("claw loop-fusion group(  )");
    assertNull(ClawPragma.getGroupOptionValue(pragma1));
    pragma1.setData("claw loop-fusion");
    assertNull(ClawPragma.getGroupOptionValue(pragma1));
  }

  @Test
  public void getExtractFusionOptionTest() {
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) fusion group(j1) parallel acc(loop seq)");
    assertEquals("j1", ClawPragma.getExtractFusionOption(pragma1));
    pragma1.setData("claw loop-extract range(i=istart,iend) map(value1:i) " +
        "map(value2:i) parallel acc(loop seq) fusion group(j1)");
    assertEquals("j1", ClawPragma.getExtractFusionOption(pragma1));
  }

  @Test
  public void extractRangeInformationTest(){
    XcodeProgram program = XelementHelper.createNewProgram();
    Xpragma pragma1 = XelementHelper.createEmpty(Xpragma.class, program);
    assertNotNull(pragma1);
    pragma1.setData("claw loop-extract range(i=istart,iend)");
    ClawRange r1 = ClawPragma.extractRangeInformation(pragma1);
    assertNotNull(r1);
    assertEquals("i", r1.getInductionVar());
    assertEquals("istart", r1.getLowerBound());
    assertEquals("iend", r1.getUpperBound());
    assertEquals(Constant.DEFAULT_STEP_VALUE, r1.getStep());

    pragma1.setData("claw loop-extract range(i=1,10)");
    ClawRange r2 = ClawPragma.extractRangeInformation(pragma1);
    assertNotNull(r2);
    assertEquals("i", r2.getInductionVar());
    assertEquals("1", r2.getLowerBound());
    assertEquals("10", r2.getUpperBound());
    assertEquals(Constant.DEFAULT_STEP_VALUE, r2.getStep());

    pragma1.setData("claw loop-extract range(i=1,10,2 )");
    ClawRange r3 = ClawPragma.extractRangeInformation(pragma1);
    assertNotNull(r3);
    assertEquals("i", r3.getInductionVar());
    assertEquals("1", r3.getLowerBound());
    assertEquals("10", r3.getUpperBound());
    assertEquals("2", r3.getStep());

    pragma1.setData("claw loop-extract range(i=istart, iend , 10)");
    ClawRange r4 = ClawPragma.extractRangeInformation(pragma1);
    assertNotNull(r4);
    assertEquals("i", r4.getInductionVar());
    assertEquals("istart", r4.getLowerBound());
    assertEquals("iend", r4.getUpperBound());
    assertEquals("10", r4.getStep());

    pragma1.setData("claw loop-extract range()");
    ClawRange r5 = ClawPragma.extractRangeInformation(pragma1);
    assertNull(r5);

    pragma1.setData("claw loop-extract range(i=istart,iend) map(a:j1)");
    ClawRange r6 = ClawPragma.extractRangeInformation(pragma1);
    assertNotNull(r6);
    assertEquals("i", r6.getInductionVar());
    assertEquals("istart", r6.getLowerBound());
    assertEquals("iend", r6.getUpperBound());
    assertEquals(Constant.DEFAULT_STEP_VALUE, r1.getStep());
  }
}
