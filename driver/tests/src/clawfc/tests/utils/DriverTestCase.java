/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests.utils;

import static clawfc.Utils.copy;
import static clawfc.Utils.fileExists;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import clawfc.Driver;
import clawfc.Utils;
import clawfc.utils.ByteArrayIOStream;
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
        TMP_DIR.toFile().deleteOnExit();
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

    protected static class Result
    {
        public final String stderr;
        public final String stdout;

        public Result(String stderr, String stdout)
        {
            this.stderr = stderr;
            this.stdout = stdout;
        }
    }

    static void resetStdStreams()
    {
        System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }

    protected static Result run(String[] args, boolean rethrowException) throws Exception
    {
        Result res = null;
        ByteArrayIOStream stdErr = new ByteArrayIOStream();
        ByteArrayIOStream stdOut = new ByteArrayIOStream();
        try
        {
            System.setErr(new PrintStream(stdErr));
            System.setOut(new PrintStream(stdOut));
            Driver.run(args);
        } catch (Exception e)
        {
            if (rethrowException)
            {
                copy(stdOut.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.out));
                copy(stdErr.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.err));
                throw e;
            }
        } finally
        {
            res = new Result(Utils.collectIntoString(stdErr.getAsInputStreamUnsafe()),
                    Utils.collectIntoString(stdOut.getAsInputStreamUnsafe()));
            resetStdStreams();
        }
        return res;
    }

    protected static Result run(String[] args) throws Exception
    {
        return run(args, true);
    }

    public static String readTxt(Path path) throws Exception
    {
        assertTrue(fileExists(path));
        return new String(Files.readAllBytes(Paths.get(path.toString())), StandardCharsets.UTF_8);
    }

    public boolean equalsTxtFiles(Path res, Path ref) throws Exception
    {

        String refTxt = readTxt(ref);
        return txtFileEqualsTxt(res, refTxt);
    }

    public boolean txtFileEqualsTxt(Path res, String refTxt) throws Exception
    {
        String resTxt = readTxt(res);
        if (!resTxt.equals(refTxt))
        {
            return false;
        }
        return true;
    }

    public static void assertTxtFileEqualsTxt(Path res, String refTxt) throws Exception
    {
        String resTxt = readTxt(res);
        assertEquals(refTxt, resTxt);
    }

    public static void assertEqualsTxtFiles(Path res, Path ref) throws Exception
    {

        String refTxt = readTxt(ref);
        assertTxtFileEqualsTxt(res, refTxt);
    }

    public static String removeTime(String in)
    {
        return in.replaceFirst("time=\".*\"", "time=\"\"");
    }

    public static String removeType(String in)
    {
        return in.replaceAll("type=\".*\"", "type=\"\"");
    }
}