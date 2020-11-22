/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests.utils;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;

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

    protected Result run(String[] args) throws Exception
    {
        Result res;
        try
        {
            ByteArrayIOStream stdErr = new ByteArrayIOStream();
            ByteArrayIOStream stdOut = new ByteArrayIOStream();
            System.setErr(new PrintStream(stdErr));
            System.setOut(new PrintStream(stdOut));
            Driver.run(args);
            res = new Result(Utils.collectIntoString(stdErr.getAsInputStreamUnsafe()),
                    Utils.collectIntoString(stdOut.getAsInputStreamUnsafe()));
        } finally
        {
            System.setErr(new PrintStream(new FileOutputStream(FileDescriptor.err)));
            System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));
        }
        return res;
    }
}