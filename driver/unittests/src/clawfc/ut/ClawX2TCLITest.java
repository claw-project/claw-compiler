/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;

import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;

import claw.ClawX2T;
import clawfc.Utils;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class ClawX2TCLITest extends TestCase
{
    public void testPrintVersion() throws Exception
    {
        final String[] args = new String[] { "--version" };
        Result res = run(args);
        assertTrue(res.stdout.contains(claw.ClawVersion.VERSION));
    }

    public void testPrintTargets() throws Exception
    {
        final String[] args = new String[] { "--target-list" };
        Result res = run(args);
        assertTrue(res.stdout.contains("cpu"));
        assertTrue(res.stdout.contains("gpu"));
    }

    public void testPrintDirectives() throws Exception
    {
        final String[] args = new String[] { "--directive-list" };
        Result res = run(args);
        assertTrue(res.stdout.contains("openacc"));
        assertTrue(res.stdout.contains("openmp"));
    }

    public void testConfiguration() throws Exception
    {
        final String[] args = new String[] { "--show-config" };
        Result res = run(args);
        assertTrue(res.stdout.contains("CLAW Compiler configuration"));
        assertTrue(res.stdout.contains("claw-low-level-set"));
        assertTrue(res.stdout.contains("claw-high-level-set"));
        assertTrue(res.stdout.contains("claw-internal-set"));
    }

    public void testConfigurationFile() throws Exception
    {
        final Path INPUT_CFG_FILE = RES_DIR.resolve("ClawCX2T/input_cfg_file/claw-cfg.xml");
        final String[] args = new String[] { "--show-config", "-c", INPUT_CFG_FILE.toString() };
        Result res = run(args);
        assertTrue(res.stdout.contains("Default directive: openmp"));
        assertTrue(res.stdout.contains("claw-low-level-set"));
        assertTrue(res.stdout.contains("claw-high-level-set"));
        assertTrue(!res.stdout.contains("claw-internal-set"));
    }

    public void testConfigurationDir() throws Exception
    {
        final Path INPUT_CFG_DIR = RES_DIR.resolve("ClawCX2T/input_cfg_dir");
        final String[] args = new String[] { "--show-config", "-cp", INPUT_CFG_DIR.toString() };
        Result res = run(args);
        assertTrue(res.stdout.contains("Default directive: openacc"));
        assertTrue(res.stdout.contains("claw-high-level-set"));
        assertTrue(!res.stdout.contains("claw-low-level-set"));
        assertTrue(!res.stdout.contains("claw-internal-set"));
    }

    public void testTarget() throws Exception
    {
        {
            final String[] args = new String[] { "--show-config", "-t", "cpu" };
            Result res = run(args);
            assertTrue(res.stdout.contains("Default target: cpu"));
        }
        {
            final String[] args = new String[] { "--show-config", "-t", "gpu" };
            Result res = run(args);
            assertTrue(res.stdout.contains("Default target: gpu"));
        }
    }

    public void testDirective() throws Exception
    {
        {
            final String[] args = new String[] { "--show-config", "-dir", "openmp" };
            Result res = run(args);
            assertTrue(res.stdout.contains("Default directive: openmp"));
        }
        {
            final String[] args = new String[] { "--show-config", "-dir", "openacc" };
            Result res = run(args);
            assertTrue(res.stdout.contains("Default directive: openacc"));
        }
    }

    public void testInputFile() throws Exception
    {
        final Path INPUT_FILE = RES_DIR.resolve("ClawCX2T/run/input/original_code.xml");
        {
            final String[] args = new String[] { INPUT_FILE.toString() };
            Result res = run(args);
            // final String refStr = Utils.collectIntoString(INPUT_FILE);
            final String resStr = res.stdout;
            assertTrue(res.stdout.contains("XcodeProgram"));
            // assertTrue(res.stdout.contains("Default directive: openmp"));
        }
    }

    public void testXcodeMLOutput() throws Exception
    {
        final Path INPUT_FILE = RES_DIR.resolve("ClawCX2T/run/input/original_code.xml");
        final Path OUTPUT_FILE = TMP_DIR.resolve("out.xml");
        {
            final String[] args = new String[] { INPUT_FILE.toString(), "-o", OUTPUT_FILE.toString() };
            Result res = run(args);
            final String outStr = collectIntoString(OUTPUT_FILE);
            assertTrue(outStr.contains("XcodeProgram"));
            // assertTrue(res.stdout.contains("Default directive: openmp"));
        }
    }

    public void testOutput() throws Exception
    {
        final Path INPUT_FILE = RES_DIR.resolve("ClawCX2T/run/input/original_code.xml");
        final Path REF_FILE = RES_DIR.resolve("ClawCX2T/run/reference/reference.f90");
        final Path OUTPUT_FILE = TMP_DIR.resolve("out.f90");
        {
            final String[] args = new String[] { INPUT_FILE.toString(), "-f", OUTPUT_FILE.toString() };
            Result res = run(args);
            // final String refStr = Utils.collectIntoString(INPUT_FILE);
            final String resStr = res.stdout;
            assertTrue(res.stdout.contains("XcodeProgram"));
            final String outStr = collectIntoString(OUTPUT_FILE);
            final String refStr = collectIntoString(REF_FILE);
            assertEquals(refStr, outStr);
        }
    }

    public void testTransformReport() throws Exception
    {
        final Path INPUT_FILE = RES_DIR.resolve("ClawCX2T/run/input/original_code.xml");
        final Path OUTPUT_FILE = TMP_DIR.resolve("report.txt");
        {
            final String[] args = new String[] { INPUT_FILE.toString(), "-r", OUTPUT_FILE.toString() };
            Result res = run(args);
            final String outStr = collectIntoString(OUTPUT_FILE);
            // System.out.println(outStr);
            assertTrue(outStr.contains("CLAW Transformation Report"));
        }
    }

    protected Path TMP_DIR;
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

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

    protected static Result run(String[] args) throws Exception
    {
        Result res;
        ByteArrayIOStream stdErr = new ByteArrayIOStream();
        ByteArrayIOStream stdOut = new ByteArrayIOStream();
        try
        {
            System.setErr(new PrintStream(stdErr));
            System.setOut(new PrintStream(stdOut));
            ClawX2T.main(args);
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
}
