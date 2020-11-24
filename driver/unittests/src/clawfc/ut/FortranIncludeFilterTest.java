/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import clawfc.Utils;
import clawfc.depscan.FortranIncludeFilter;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class FortranIncludeFilterTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    public void testParallelBuildOrder() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("include_filter/input");
        final Path REF_DIR = RES_DIR.resolve("include_filter/reference");
        final Path IN_FILEPATH = IN_DIR.resolve("1.f90");
        final Path REF_FILEPATH2 = REF_DIR.resolve("1.f90");

        InputStream in = Files.newInputStream(IN_FILEPATH);
        ByteArrayIOStream out = new ByteArrayIOStream();
        FortranIncludeFilter filter = new FortranIncludeFilter();
        assertTrue(filter.run(in, out));
        String refStr = Utils.collectIntoString(Files.newInputStream(REF_FILEPATH2));
        String resStr = Utils.collectIntoString(out.getAsInputStreamUnsafe());
        assertEquals(refStr, resStr);
    }
}
