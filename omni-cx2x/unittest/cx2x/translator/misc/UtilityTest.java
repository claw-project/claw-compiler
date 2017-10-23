/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.misc;


import cx2x.translator.common.Utility;
import cx2x.translator.language.accelerator.AcceleratorDirective;
import cx2x.translator.language.base.Target;
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

  @Test
  public void formattedModuleFilePrefixTest() {
    // .[directive].[target].claw
    assertEquals(".openacc.gpu.claw", Utility.
        formattedModuleFilePrefix(Target.GPU, AcceleratorDirective.OPENACC));
    assertEquals(".openmp.cpu.claw", Utility.
        formattedModuleFilePrefix(Target.CPU, AcceleratorDirective.OPENMP));
    assertEquals(".none.cpu.claw", Utility.
        formattedModuleFilePrefix(Target.CPU, AcceleratorDirective.NONE));
    assertEquals(".openmp.mic.claw", Utility.
        formattedModuleFilePrefix(Target.MIC, AcceleratorDirective.OPENMP));
    assertEquals(".none.fpga.claw", Utility.
        formattedModuleFilePrefix(Target.FPGA, AcceleratorDirective.NONE));
    assertEquals(".claw", Utility.
        formattedModuleFilePrefix(null, null));
    assertEquals(".openacc.claw", Utility.
        formattedModuleFilePrefix(null, AcceleratorDirective.OPENACC));
    assertEquals(".gpu.claw", Utility.
        formattedModuleFilePrefix(Target.GPU, null));
  }

}
