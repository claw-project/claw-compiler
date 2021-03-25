/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import clawfc.CommentTrailingBackslashRecognizer;
import clawfc.TrailingBackslashCommentsFilter;
import clawfc.Utils;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class TrailingBackslashCommentsFilterTest extends TestCase
{
    TrailingBackslashCommentsFilter filter;

    @Override
    protected void setUp() throws Exception
    {
        filter = new TrailingBackslashCommentsFilter();
    }

    private void verifyTrailingBSRecognizer(String in, String expectedOut) throws Exception
    {
        ByteArrayIOStream inStrm = new ByteArrayIOStream();
        Utils.copy(new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII)), inStrm);
        ByteArrayIOStream outStrm = new ByteArrayIOStream();
        CommentTrailingBackslashRecognizer filter = new CommentTrailingBackslashRecognizer();
        AsciiArrayIOStream buf = new AsciiArrayIOStream(in);
        final String res = filter.parse(buf.getAsInputStreamUnsafe());
        assertEquals(res, expectedOut);
    }

    public void testCommentsExtraction() throws Exception
    {
        verifyTrailingBSRecognizer("!\n", "!\n");
        verifyTrailingBSRecognizer("!\r\n", "!\r\n");
        verifyTrailingBSRecognizer("!bla\n", "!bla\n");
        verifyTrailingBSRecognizer("!bla\\\n", "!bla\n");
        verifyTrailingBSRecognizer("!bla \\\n", "!bla \n");
        verifyTrailingBSRecognizer("!bla \\\r\n", "!bla \r\n");
        verifyTrailingBSRecognizer("!bla \\  \\\r\n", "!bla \r\n");
        verifyTrailingBSRecognizer("!bla \\ | \\\r\n", "!bla \\ | \r\n");
    }
}
