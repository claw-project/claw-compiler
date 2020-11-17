/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import java.nio.file.Files;
import java.nio.file.Path;

import clawfc.Utils;
import junit.framework.TestCase;

public abstract class DriverTestCase extends TestCase
{
    protected Path TMP_DIR;
    protected final Path RES_DIR = clawfc.tests.Resources.DIR;
    protected final Path DRIVER_PATH = clawfc.tests.Resources.DRIVER_PATH();

    @Override
    protected void setUp() throws Exception
    {
        TMP_DIR = Files.createTempDirectory(null);
        assertNotNull(TMP_DIR);
    }

    @Override
    protected void tearDown() throws Exception
    {
        if (TMP_DIR != null)
        {
            Utils.removeDir(TMP_DIR);
            TMP_DIR = null;
        }
    }
}