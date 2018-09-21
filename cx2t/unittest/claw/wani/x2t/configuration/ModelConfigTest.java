/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import helper.TestConstant;
import org.junit.Test;

import static junit.framework.TestCase.fail;

/**
 * Test method of the ModelConfig class
 *
 * @author clementval
 */
public class ModelConfigTest {

  @Test
  public void loadTest() {
    ModelConfig cfg = ModelConfig.get();
    try {
      cfg.load(TestConstant.TEST_MODEL_CONFIG);
    } catch(Exception ignored) {
      fail();
    }
  }
}
