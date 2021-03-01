/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.toInputStream;

import java.io.IOException;
import java.nio.file.Path;

import clawfc.AddIgnoreDirectiveFilter;
import clawfc.IgnoreDirectiveRecognizer;
import clawfc.RemoveIgnoreDirectiveFilter;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class IgnoreDirectiveFilterTest extends TestCase
{
    void verifyRecognizer(IgnoreDirectiveRecognizer recognizer, final String input, final String ref)
            throws IOException, Exception
    {
        final String res = recognizer.parse(toInputStream(input));
        assertEquals(ref, res);
    }

    public void testRecognizer() throws Exception
    {
        IgnoreDirectiveRecognizer recognizer = new IgnoreDirectiveRecognizer();
        verifyRecognizer(recognizer, "!$claw ignore |text|\n", "text");
        verifyRecognizer(recognizer, " \t!$claw ignore |text|\n", "text");
        verifyRecognizer(recognizer, " \t!$claw\t ignore |text|\n", "text");
        verifyRecognizer(recognizer, " \t!$claw\t ignore\t |text|\n", "text");
        verifyRecognizer(recognizer, " \t!$claw\t ignore\t |text|\t \n", "text");
        verifyRecognizer(recognizer, " \t!$claw\t ignore\t ||text|\t \n", "|text");
        verifyRecognizer(recognizer, " \t!$claw\t ignore\t ||text||\t \n", "|text|");
    }

    final static Path RES_DIR = clawfc.ut.Resources.DIR;
    final static Path INPUT_DIR = RES_DIR.resolve("ignore_filter/input");
    final static Path REF_DIR = RES_DIR.resolve("ignore_filter/reference");

    void verifyAddFilter(AddIgnoreDirectiveFilter filter, Path inputFilePath, Path refFilePath) throws Exception
    {
        AsciiArrayIOStream input = new AsciiArrayIOStream(inputFilePath);
        AsciiArrayIOStream output = new AsciiArrayIOStream();
        filter.run(input.getAsInputStreamUnsafe(), output);
        AsciiArrayIOStream ref = new AsciiArrayIOStream(refFilePath);
        String resStr = collectIntoString(output.getAsInputStreamUnsafe());
        String refStr = collectIntoString(ref.getAsInputStreamUnsafe());
        assertEquals(refStr, resStr);
    }

    public void testAddFilter() throws Exception
    {
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("add.f90");
        final Path REF_FILEPATH = REF_DIR.resolve("add.f90");
        final Path INPUT_FILEPATH2 = INPUT_DIR.resolve("add2.f90");
        final Path REF_FILEPATH2 = REF_DIR.resolve("add2.f90");
        AddIgnoreDirectiveFilter filter = new AddIgnoreDirectiveFilter();
        verifyAddFilter(filter, INPUT_FILEPATH, REF_FILEPATH);
        verifyAddFilter(filter, INPUT_FILEPATH2, REF_FILEPATH2);
    }

    public void testAddFilterUnclosedBlockError() throws Exception
    {
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("unclosed_error.f90");
        AddIgnoreDirectiveFilter filter = new AddIgnoreDirectiveFilter();
        AsciiArrayIOStream input = new AsciiArrayIOStream(INPUT_FILEPATH);
        AsciiArrayIOStream output = new AsciiArrayIOStream();
        boolean exCaught = false;
        try
        {
            filter.run(input.getAsInputStreamUnsafe(), output);
        } catch (Exception e)
        {
            final String errMsg = e.getMessage();
            exCaught = true;
            assertTrue(errMsg.contains("Unclosed ignore directive at line 2"));
        }
        assertTrue(exCaught);
    }

    public void testAddFilterUnopenBlockError() throws Exception
    {
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("unopened_error.f90");
        AddIgnoreDirectiveFilter filter = new AddIgnoreDirectiveFilter();
        AsciiArrayIOStream input = new AsciiArrayIOStream(INPUT_FILEPATH);
        AsciiArrayIOStream output = new AsciiArrayIOStream();
        boolean exCaught = false;
        try
        {
            filter.run(input.getAsInputStreamUnsafe(), output);
        } catch (Exception e)
        {
            final String errMsg = e.getMessage();
            exCaught = true;
            assertTrue(errMsg.contains("\"Ignore end\" directive at line 3 with no ignore block open"));
        }
        assertTrue(exCaught);
    }

    void verifyRemoveFilter(RemoveIgnoreDirectiveFilter filter, Path inputFilePath, Path refFilePath) throws Exception
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
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("remove.f90");
        final Path REF_FILEPATH = REF_DIR.resolve("remove.f90");
        final Path INPUT_FILEPATH2 = INPUT_DIR.resolve("remove2.f90");
        final Path REF_FILEPATH2 = REF_DIR.resolve("remove2.f90");
        RemoveIgnoreDirectiveFilter filter = new RemoveIgnoreDirectiveFilter();
        verifyRemoveFilter(filter, INPUT_FILEPATH, REF_FILEPATH);
        verifyRemoveFilter(filter, INPUT_FILEPATH2, REF_FILEPATH2);
    }
}
