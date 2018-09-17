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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

/**
 * Test method of the Configuration class
 *
 * @author clementval
 */
public class ConfigurationTest {

  private static final int MAX_COLUMN = 80;

  @Test
  public void readConfigTest() {
    try {

      Configuration conf = Configuration.get();
      assertNotNull(conf);
      conf.load(TestConstant.TEST_CONFIG_PATH, null, null, null, MAX_COLUMN);
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



    } catch(Exception e) {
      fail();
    }
  }
}
