/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.nio.file.Path;

import org.junit.Test;

import claw.wani.ClawVersion;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.directive.configuration.OpenAccConfiguration;
import claw.tatsu.directive.generator.OpenAcc;
import helper.TestConstant;
import helper.Utils.TestConfiguration;
import helper.Utils.TestContext;

/**
 * Test method of the Configuration class
 *
 * @author clementval
 */
public class ConfigurationTest
{

    private static final String DUMMY_KEY = "dummy";
    private static final String DUMMY_VALUE = "dummyValue";

    private static final int MAX_COLUMN = 80;

    void readConfig(Path cfgDirPath)
    {
        try
        {
            Configuration conf = new TestConfiguration();
            assertNotNull(conf);
            Context context = new TestContext();
            assertNotNull(context);
            conf = Configuration.load(cfgDirPath, null, null, null, null, Integer.valueOf(MAX_COLUMN), context);

            assertNotNull(context.getGenerator());
            assertNotNull(conf.accelerator());
            assertEquals(MAX_COLUMN, context.getMaxColumns());

            assertFalse(conf.isForcePure());
            conf.setForcePure();
            assertTrue(conf.isForcePure());

            int[] majorMinor = conf.getMajorMinor("0.3");
            assertEquals(0, majorMinor[0]);
            assertEquals(3, majorMinor[1]);

            try
            {
                conf.checkVersion("0.2");
                fail();
            } catch (Exception ignored)
            {
            }

            try
            {
                conf.getMajorMinor("sdjhsajkd");
                fail();
            } catch (Exception ignored)
            {
            }

            try
            {
                conf.checkVersion(ClawVersion.VERSION);
            } catch (Exception ignored)
            {
                fail();
            }

            assertTrue(conf.accelerator() instanceof OpenAccConfiguration);
            assertTrue(context.getGenerator() instanceof OpenAcc);

            assertSame(Target.GPU, conf.getCurrentTarget());
            assertSame(CompilerDirective.OPENACC, conf.getCurrentDirective());

            assertEquals(Configuration.CPU_STRATEGY_FUSION, conf.getParameter(Configuration.CPU_STRATEGY));

            conf.overrideConfigurationParameter(Configuration.CPU_STRATEGY, Configuration.CPU_STRATEGY_SINGLE);

            assertEquals(Configuration.CPU_STRATEGY_SINGLE, conf.getParameter(Configuration.CPU_STRATEGY));

            assertNull(conf.getParameter(DUMMY_KEY));
            conf.overrideConfigurationParameter(DUMMY_KEY, DUMMY_VALUE);
            assertEquals(DUMMY_VALUE, conf.getParameter(DUMMY_KEY));
        } catch (Exception e)
        {
            fail();
        }
    }

    @Test
    public void readConfigTest()
    {
        readConfig(null);// Read from jar
        readConfig(TestConstant.TEST_CONFIG_PATH);// Read from dir
    }
}
