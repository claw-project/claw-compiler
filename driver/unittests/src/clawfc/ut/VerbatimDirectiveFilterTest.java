/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;

import java.nio.file.Path;

import clawfc.RemoveVerbatimDirectiveFilter;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class VerbatimDirectiveFilterTest extends TestCase
{

    final static Path RES_DIR = clawfc.ut.Resources.DIR;
    final static Path INPUT_DIR = RES_DIR.resolve("verbatim_filter/input");
    final static Path REF_DIR = RES_DIR.resolve("verbatim_filter/reference");

    void verifyVerbatimFilter(RemoveVerbatimDirectiveFilter filter, Path inputFilePath, Path refFilePath)
            throws Exception
    {
        AsciiArrayIOStream input = new AsciiArrayIOStream(inputFilePath);
        AsciiArrayIOStream output = new AsciiArrayIOStream();
        filter.run(input.getAsInputStreamUnsafe(), output);
        AsciiArrayIOStream ref = new AsciiArrayIOStream(refFilePath);
        String resStr = collectIntoString(output.getAsInputStreamUnsafe());
        String refStr = collectIntoString(ref.getAsInputStreamUnsafe());
        assertEquals(refStr, resStr);
    }

    public void testRemoveFilter() throws Exception
    {
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("1.f90");
        final Path REF_FILEPATH = REF_DIR.resolve("1.f90");
        RemoveVerbatimDirectiveFilter filter = new RemoveVerbatimDirectiveFilter();
        verifyVerbatimFilter(filter, INPUT_FILEPATH, REF_FILEPATH);
    }
}
