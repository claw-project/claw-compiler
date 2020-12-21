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

    protected class Result
    {
        public final String stderr;
        public final String stdout;

        public Result(String stderr, String stdout)
        {
            this.stderr = stderr;
            this.stdout = stdout;
        }
    }

    void resetStdStreams()
    {
        System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
        System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
    }

    protected Result run(String[] args) throws Exception
    {
        Result res;
        ByteArrayIOStream stdErr = new ByteArrayIOStream();
        ByteArrayIOStream stdOut = new ByteArrayIOStream();
        try
        {
            System.setErr(new PrintStream(stdErr));
            System.setOut(new PrintStream(stdOut));
            Driver.run(args);
            res = new Result(Utils.collectIntoString(stdErr.getAsInputStreamUnsafe()),
                    Utils.collectIntoString(stdOut.getAsInputStreamUnsafe()));
        } catch (Exception e)
        {
            copy(stdOut.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.out));
            copy(stdErr.getAsInputStreamUnsafe(), new FileOutputStream(FileDescriptor.err));
            throw e;
        } finally
        {
            resetStdStreams();
        }
        return res;
    }

    public String readTxt(Path path) throws Exception
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

    public void assertTxtFileEqualsTxt(Path res, String refTxt) throws Exception
    {
        String resTxt = readTxt(res);
        assertEquals(refTxt, resTxt);
    }

    public void assertEqualsTxtFiles(Path res, Path ref) throws Exception
    {

        String refTxt = readTxt(ref);
        assertTxtFileEqualsTxt(res, refTxt);
    }
}