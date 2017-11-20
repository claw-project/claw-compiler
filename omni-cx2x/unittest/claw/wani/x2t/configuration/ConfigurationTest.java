/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import org.junit.Test;

/**
 * Test method of the Configuration class
 *
 * @author clementval
 */
public class ConfigurationTest {

  @Test
  public void readConfigTest() {
    /*try {
      Configuration configuration = new Configuration(TestConstant.TEST_CONFIG, null);
      assertNotNull(configuration.getAcceleratorGenerator());
      assertNotNull(configuration.openACC());

      configuration.setMaxColumns(80);
      assertEquals(80, configuration.getMaxColumns());

      assertFalse(configuration.isForcePure());
      configuration.setForcePure();
      assertTrue(configuration.isForcePure());

      int[] majorMinor = configuration.getMajorMinor("0.3");
      assertEquals(0, majorMinor[0]);
      assertEquals(3, majorMinor[1]);

      try {
        configuration.checkVersion("0.2");
        fail();
      } catch(Exception ignored) {
      }

      try {
        //noinspection unused
        int[] majorMinor2 = configuration.getMajorMinor("sdjhsajkd");
        fail();
      } catch(Exception ignored) {
      }

      try {
        configuration.checkVersion("0.4");
      } catch(Exception ignored) {
        fail();
      }


      assertEquals(Target.GPU, configuration.getCurrentTarget());
      configuration.setUserDefinedTarget(Target.CPU.toString());
      assertEquals(Target.CPU, configuration.getCurrentTarget());

      assertEquals(CompilerDirective.OPENACC, configuration.getCurrentDirective());
      configuration.setUserDefineDirective(CompilerDirective.OPENMP.toString());
      assertEquals(CompilerDirective.OPENMP, configuration.getCurrentDirective());

      configuration.displayConfig();

    } catch(Exception e) {
      fail();
    }*/
  }
}
