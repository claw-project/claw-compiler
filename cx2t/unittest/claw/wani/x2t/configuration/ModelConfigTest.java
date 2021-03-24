/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Collections;

import org.junit.Test;

import claw.tatsu.xcodeml.abstraction.BoundDefinition;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import helper.TestConstant;

/**
 * Test method of the ModelConfig class
 *
 * @author clementval
 */
public class ModelConfigTest
{

    @Test
    public void loadTest()
    {
        ModelConfig cfg = new ModelConfig();
        try
        {
            cfg.load(TestConstant.TEST_MODEL_CONFIG);

            assertEquals("ModelX", cfg.getName());
            assertEquals(2, cfg.getNbDimensions());
            assertEquals(4, cfg.getNbLayouts());

            // Check correctness of "horizontal" dimension information
            DimensionDefinition hori = cfg.getDimension("horizontal");
            assertNotNull(hori);

            assertBound(hori.getLowerBound(), 1);
            assertBound(hori.getUpperBound(), "nproma");
            assertBound(hori.getIterationLowerBound(), "pstart");
            assertBound(hori.getIterationUpperBound(), "pend");
            assertBound(hori.getIterationStep(), 1);

            // Check correctness of "vertical" dimension information
            DimensionDefinition vertical = cfg.getDimension("vertical");
            assertNotNullDimension(vertical);
            assertBound(vertical.getLowerBound(), 1);
            assertBound(vertical.getUpperBound(), "klev");
            assertEquals(vertical.getLowerBound(), vertical.getIterationLowerBound());
            assertEquals(vertical.getUpperBound(), vertical.getIterationUpperBound());
            assertBound(vertical.getIterationStep(), 1);

            assertNull(cfg.getDimension("unknown"));
            assertNull(cfg.getDimension(null));

            assertNotNull(cfg.getDefaultLayout());
            assertFalse(cfg.hasLayout(null));
            assertLayout(cfg, "cpu", 1);
            assertLayout(cfg, "gpu", 1);
            assertLayout(cfg, "radiation", 1);

            assertTrue(cfg.getLayout("unknown").isEmpty());
            assertTrue(cfg.getLayout(null).isEmpty());

        } catch (Exception ignored)
        {
            fail();
        }
    }

    @Test
    public void dimensionsTest()
    {
        ModelConfig cfg = new ModelConfig();
        assertEquals(0, cfg.getNbDimensions());

        DimensionDefinition d = new DimensionDefinition("dim1", "0", "ndim1");
        assertFalse(cfg.hasDimension(d.getIdentifier()));
        cfg.putDimension(d);
        assertTrue(cfg.hasDimension(d.getIdentifier()));
        assertEquals(d, cfg.getDimension(d.getIdentifier()));
    }

    @Test
    public void layoutTest()
    {
        ModelConfig cfg = new ModelConfig();
        assertEquals(0, cfg.getNbLayouts());
        DimensionDefinition d = new DimensionDefinition("dim1", "0", "ndim1");
        assertTrue(cfg.getDefaultLayout().isEmpty());
        assertFalse(cfg.hasDimension(d.getIdentifier()));
        cfg.putDefaultLayout(Collections.singletonList(d));
        assertFalse(cfg.getDefaultLayout().isEmpty());
        assertTrue(cfg.hasDimension(d.getIdentifier()));
        assertEquals(d, cfg.getDimension(d.getIdentifier()));
        assertEquals(1, cfg.getDefaultLayout().size());
        assertEquals(d, cfg.getDefaultLayout().get(0));
    }

    @Test
    public void wrongPathTest()
    {
        try
        {
            ModelConfig cfg = new ModelConfig();
            cfg.load(Paths.get("dummy.toml"));
            fail();
        } catch (Exception ignored)
        {
        }
    }

    @Test
    public void malformattedTomlTest()
    {
        try
        {
            ModelConfig cfg = new ModelConfig();
            cfg.load(TestConstant.TEST_MODEL_CONFIG_MALFORMATTED);
            fail();
        } catch (Exception ex)
        {
            assertEquals(ModelConfig.ERR_MALFORMATTED, ex.getMessage());
        }
    }

    @Test
    public void errorCheckTest()
    {

        String layout1 = "layout1";

        // No dimension defined
        StringBuilder config = new StringBuilder();
        config.append("[model]").append("\n");
        config.append("name = \"model_name\"").append("\n");
        assertError(config, String.format(ModelConfig.ERR_NO_DIMENSIONS, "model_name"));

        // No size defined
        config.append("[[dimensions]]").append("\n");
        config.append("id=\"dim1\"").append("\n");
        assertError(config, String.format(ModelConfig.ERR_NO_SIZE, "dim1"));

        // No upper bound defined
        config.append("[dimensions.size]").append("\n");
        config.append("lower = 1").append("\n");
        assertError(config, String.format(ModelConfig.ERR_NO_UPPER, "dim1"));

        config.append("upper = \"nproma\"").append("\n");

        assertError(config, String.format(ModelConfig.ERR_NO_LAYOUTS, "model_name"));

        config.append("[[layouts]]").append("\n");
        assertError(config, ModelConfig.ERR_LAYOUT_NO_ID);

        config.append("id=\"").append(layout1).append("\"").append("\n");
        assertError(config, String.format(ModelConfig.ERR_LAYOUT_NO_POSITION, layout1));

        String positionNotAvail = "position = [ \"dim2\" ] ";

        config.append(positionNotAvail).append("\n");
        assertError(config, String.format(ModelConfig.ERR_DIM_NOT_AVAIL, "dim2", layout1));

        config.delete(config.length() - positionNotAvail.length() - 1, config.length());

        config.append("position = [ \"dim1\" ]").append("\n");

        assertError(config, String.format(ModelConfig.ERR_NO_BASE_DIM, "layout1"));
    }

    private void assertError(StringBuilder config, String expectedError)
    {
        try
        {
            ModelConfig cfg = new ModelConfig();
            cfg.load(getStreamFromString(config.toString()));
            fail();
        } catch (Exception ex)
        {
            assertEquals(expectedError, ex.getMessage());
        }
    }

    private InputStream getStreamFromString(String value)
    {
        return new ByteArrayInputStream(value.getBytes(StandardCharsets.UTF_8));
    }

    private void assertLayout(ModelConfig cfg, String layoutId, int expectedNbDimensions)
    {
        assertNotNull(cfg.getLayout(layoutId));
        assertTrue(cfg.hasLayout(layoutId));
        assertEquals(expectedNbDimensions, cfg.getLayout(layoutId).size());
    }

    private void assertBound(BoundDefinition bd, int expectedValue)
    {
        assertFalse(bd.isVar());
        assertEquals(expectedValue, bd.getIntValue());
    }

    private void assertBound(BoundDefinition bd, String expectedValue)
    {
        assertTrue(bd.isVar());
        assertEquals(expectedValue, bd.getValue());
    }

    private void assertNotNullDimension(DimensionDefinition dim)
    {
        assertNotNull(dim);
        assertNotNull(dim.getLowerBound());
        assertNotNull(dim.getUpperBound());
        assertNotNull(dim.getIterationLowerBound());
        assertNotNull(dim.getIterationUpperBound());
        assertNotNull(dim.getIterationStep());
    }

}
