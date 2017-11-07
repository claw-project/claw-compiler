/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.config;

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
      Configuration config = new Configuration(TestConstant.TEST_CONFIG, null);
      assertNotNull(config.getAcceleratorGenerator());
      assertNotNull(config.openACC());

      config.setMaxColumns(80);
      assertEquals(80, config.getMaxColumns());

      assertFalse(config.isForcePure());
      config.setForcePure();
      assertTrue(config.isForcePure());

      int[] majorMinor = config.getMajorMinor("0.3");
      assertEquals(0, majorMinor[0]);
      assertEquals(3, majorMinor[1]);

      try {
        config.checkVersion("0.2");
        fail();
      } catch(Exception ignored) {
      }

      try {
        //noinspection unused
        int[] majorMinor2 = config.getMajorMinor("sdjhsajkd");
        fail();
      } catch(Exception ignored) {
      }

      try {
        config.checkVersion("0.4");
      } catch(Exception ignored) {
        fail();
      }


      assertEquals(Target.GPU, config.getCurrentTarget());
      config.setUserDefinedTarget(Target.CPU.toString());
      assertEquals(Target.CPU, config.getCurrentTarget());

      assertEquals(AcceleratorDirective.OPENACC, config.getCurrentDirective());
      config.setUserDefineDirective(AcceleratorDirective.OPENMP.toString());
      assertEquals(AcceleratorDirective.OPENMP, config.getCurrentDirective());

      config.displayConfig();

    } catch(Exception e) {
      fail();
    }*/
  }

}
