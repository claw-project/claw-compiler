/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */


package cx2x.translator.common.analysis.dependence;

import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import helper.TestConstant;
import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test the various features of the DependenceAnalysis class.
 *
 * @author clementval
 */
public class DependenceAnalysisTest {

  @Test
  public void analyzeTest() {
    File f = new File(TestConstant.TEST_DEPENDENCE);
    assertTrue(f.exists());
    XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DEPENDENCE);
    assertNotNull(xcodeml);
    List<Xnode> functions = xcodeml.matchAll(Xcode.FFUNCTIONDEFINITION);
    assertEquals(2, functions.size());

    Xnode lw_solver_noscat = functions.get(0);

    List<Xnode> loops = lw_solver_noscat.matchAll(Xcode.FDOSTATEMENT);
    assertEquals(10, loops.size());

    List<DependenceAnalysis> dependencies = new ArrayList<>();
    for(Xnode loop : loops) {
      try {
        dependencies.add(new DependenceAnalysis(loop));
      } catch(Exception e) {
        fail();
      }
    }

    assertTrue(dependencies.get(0).isIndependent());
    assertTrue(dependencies.get(1).isIndependent());
    assertTrue(dependencies.get(2).isIndependent());

    assertFalse(dependencies.get(3).isIndependent());
    assertEquals(1, dependencies.get(3).getDistanceVector());
    assertEquals(DependenceDirection.BACKWARD,
        dependencies.get(3).getDirectionVector());

    assertFalse(dependencies.get(4).isIndependent());
    assertEquals(1, dependencies.get(4).getDistanceVector());
    assertEquals(DependenceDirection.FORWARD,
        dependencies.get(4).getDirectionVector());

    assertFalse(dependencies.get(5).isIndependent());
    assertEquals(1, dependencies.get(5).getDistanceVector());
    assertEquals(DependenceDirection.FORWARD,
        dependencies.get(5).getDirectionVector());

    assertFalse(dependencies.get(6).isIndependent());
    assertEquals(1, dependencies.get(6).getDistanceVector());
    assertEquals(DependenceDirection.BACKWARD,
        dependencies.get(6).getDirectionVector());

    assertTrue(dependencies.get(7).isIndependent());
    assertTrue(dependencies.get(8).isIndependent());
    assertTrue(dependencies.get(9).isIndependent());

    for(DependenceAnalysis dep : dependencies) {
      System.out.println(dep.getInfoMsg());
    }
  }
}
