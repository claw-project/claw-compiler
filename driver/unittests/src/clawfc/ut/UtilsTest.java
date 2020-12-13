/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;

import clawfc.Utils;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.PathHashGenerator;
import clawfc.utils.SimplePathHashGenerator;
import clawfc.utils.UniquePathHashGenerator;
import junit.framework.TestCase;

public class UtilsTest extends TestCase
{
    AsciiArrayIOStream.LinesInfo getLinesInfo(String in) throws Exception
    {
        AsciiArrayIOStream strm = new AsciiArrayIOStream();
        InputStream inStrm = new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII));
        Utils.copy(inStrm, strm);
        return strm.getLinesInfo();
    }

    public void testArrayStreamRegion() throws IOException
    {
        AsciiArrayIOStream strm = new AsciiArrayIOStream();
        try (PrintStream pstrm = new PrintStream(strm))
        {
            for (int i = 0; i < 10; ++i)
            {
                pstrm.print(i);
            }
        }
        final String ref = "0123456789";
        final String testStr = collectIntoString(strm.getAsInputStreamUnsafe());
        assertEquals(ref, testStr);
        final String ref2 = "01234";
        final String testStr2 = collectIntoString(strm.getAsInputStreamUnsafe(0, 5));
        assertEquals(ref2, testStr2);
        final String ref3 = "56789";
        final String testStr3 = collectIntoString(strm.getAsInputStreamUnsafe(5, 5));
        assertEquals(ref3, testStr3);
    }

    public void testFindLineStartChrIdx()
    {
        AsciiArrayIOStream strm = new AsciiArrayIOStream();
        try (PrintStream pstrm = new PrintStream(strm))
        {
            for (int i = 0; i < 5; ++i)
            {
                pstrm.print(i);
            }
            pstrm.print('\n');
            for (int i = 6; i < 10; ++i)
            {
                pstrm.print(i);
            }
        }
        assertEquals(null, strm.findLineStartChrIdx(-1));
        for (int i = 0; i < 6; ++i)
        {
            assertEquals(Integer.valueOf(0), strm.findLineStartChrIdx(i));
        }
        for (int i = 6; i < 10; ++i)
        {
            assertEquals(Integer.valueOf(6), strm.findLineStartChrIdx(i));
        }
        assertEquals(null, strm.findLineStartChrIdx(10));
    }

    public void testLineInfo() throws Exception
    {
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("");
            assertEquals(lInfo.size(), 0);
            assertEquals(lInfo.numLines(), 0);
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla");
            assertEquals(lInfo.size(), 3);
            assertEquals(lInfo.numLines(), 1);
            assertEquals(lInfo.getLineStartByteIdx(0), 0);
            assertEquals(lInfo.lineByteLength(0), 3);
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\n");
            assertEquals(lInfo.size(), 4);
            assertEquals(lInfo.numLines(), 1);
            assertEquals(lInfo.getLineStartByteIdx(0), 0);
            assertEquals(lInfo.lineByteLength(0), 4);
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\n\n");
            assertEquals(lInfo.size(), 5);
            assertEquals(lInfo.numLines(), 2);
            assertEquals(lInfo.getLineStartByteIdx(0), 0);
            assertEquals(lInfo.lineByteLength(0), 4);
            assertEquals(lInfo.getLineStartByteIdx(1), 4);
            assertEquals(lInfo.lineByteLength(1), 1);
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\nheh");
            assertEquals(lInfo.size(), 7);
            assertEquals(lInfo.numLines(), 2);
            assertEquals(lInfo.getLineStartByteIdx(0), 0);
            assertEquals(lInfo.lineByteLength(0), 4);
            assertEquals(lInfo.getLineStartByteIdx(1), 4);
            assertEquals(lInfo.lineByteLength(1), 3);
        }
    }

    interface PathHashGeneratorFactory
    {
        public PathHashGenerator create() throws NoSuchAlgorithmException;
    }

    class SimpleHashGeneratorFactory implements PathHashGeneratorFactory
    {
        public PathHashGenerator create() throws NoSuchAlgorithmException
        {
            return new SimplePathHashGenerator();
        }
    }

    class UniqueHashGeneratorFactory implements PathHashGeneratorFactory
    {
        public PathHashGenerator create() throws NoSuchAlgorithmException
        {
            return new UniquePathHashGenerator();
        }
    }

    public void verifyPathHashGenerator(PathHashGeneratorFactory factory) throws NoSuchAlgorithmException
    {
        PathHashGenerator hashGen = factory.create();
        final Path testPath = Paths.get("/tmp/f1/f2/test.txt");
        final String ref = hashGen.generate(testPath);
        assertEquals(40, ref.length());
        for (int i = 0; i < 100; ++i)
        {
            String res = hashGen.generate(testPath);
            assertEquals(ref, res);
        }
        for (int i = 0; i < 100; ++i)
        {
            PathHashGenerator hashGen2 = factory.create();
            String res = hashGen2.generate(testPath);
            assertEquals(ref, res);
        }
        assertEquals(ref, hashGen.generate(Paths.get("/tmp/f1/../f1/f2/test.txt")));
        assertFalse(ref.equals(hashGen.generate(Paths.get("/tmp/f1/f2/test.txt_"))));
    }

    public void testSimplePathHashGenerator() throws NoSuchAlgorithmException
    {
        verifyPathHashGenerator(new SimpleHashGeneratorFactory());
    }

    public void testUniquePathHashGenerator() throws NoSuchAlgorithmException
    {
        verifyPathHashGenerator(new UniqueHashGeneratorFactory());
    }
}
