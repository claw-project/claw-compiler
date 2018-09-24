/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import claw.ClawVersion;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.generator.OpenAcc;
import helper.TestConstant;
import org.junit.Test;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.fail;
import static org.junit.Assert.*;

/**
 * Test method of the Configuration class
 *
 * @author clementval
 */
public class ConfigurationTest {

  private static final String DUMMY_KEY = "dummy";
  private static final String DUMMY_VALUE = "dummyValue";

  private static final int MAX_COLUMN = 80;

  @Test
  public void readConfigTest() {
    try {

      Configuration conf = Configuration.get();
      assertNotNull(conf);
      conf.load(TestConstant.TEST_CONFIG_PATH,
          null, null, null, null, MAX_COLUMN);
      assertNotNull(Context.get());

      assertNotNull(Context.get().getGenerator());
      assertNotNull(conf.accelerator());
      assertEquals(MAX_COLUMN, Context.get().getMaxColumns());

      assertFalse(conf.isForcePure());
      conf.setForcePure();
      assertTrue(conf.isForcePure());

      int[] majorMinor = conf.getMajorMinor("0.3");
      assertEquals(0, majorMinor[0]);
      assertEquals(3, majorMinor[1]);

      try {
        conf.checkVersion("0.2");
        fail();
      } catch(Exception ignored) {
      }

      try {
        conf.getMajorMinor("sdjhsajkd");
        fail();
      } catch(Exception ignored) {
      }

      try {
        conf.checkVersion(ClawVersion.VERSION);
      } catch(Exception ignored) {
        fail();
      }

      assertTrue(conf.accelerator() instanceof OpenAccConfiguration);
      assertTrue(Context.get().getGenerator() instanceof OpenAcc);

      assertSame(Target.GPU, conf.getCurrentTarget());
      assertSame(CompilerDirective.OPENACC, conf.getCurrentDirective());

      assertEquals(Configuration.CPU_STRATEGY_FUSION,
          Configuration.get().getParameter(Configuration.CPU_STRATEGY));

      Configuration.get().overrideConfigurationParameter(
          Configuration.CPU_STRATEGY, Configuration.CPU_STRATEGY_SINGLE);

      assertEquals(Configuration.CPU_STRATEGY_SINGLE,
          Configuration.get().getParameter(Configuration.CPU_STRATEGY));

      assertNull(Configuration.get().getParameter(DUMMY_KEY));
      Configuration.get().
          overrideConfigurationParameter(DUMMY_KEY, DUMMY_VALUE);
      assertEquals(DUMMY_VALUE, Configuration.get().getParameter(DUMMY_KEY));
    } catch(Exception e) {
      fail();
    }
  }
}
