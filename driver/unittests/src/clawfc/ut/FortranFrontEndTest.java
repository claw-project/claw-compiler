/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import clawfc.Configuration;
import clawfc.FortranFrontEnd;
import clawfc.Utils;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class FortranFrontEndTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;
    Configuration DRIVER_CFG;
    final Path IN_DIR = RES_DIR.resolve("ffront/run/input");
    final Path REF_DIR = RES_DIR.resolve("ffront/run/reference");

    @Override
    protected void setUp() throws Exception
    {
        DRIVER_CFG = new Configuration();
    }

    class RunResult
    {
        public final boolean res;
        public final String stdout;
        public final String stderr;
        public final Map<String, Path> outModFiles;

        public RunResult(boolean res, String stdout, String stderr, Map<String, Path> outModFiles)
        {
            this.res = res;
            this.stdout = stdout;
            this.stderr = stderr;
            this.outModFiles = outModFiles;
        }
    }

    RunResult run(InputStream inStrm, Path inFilename, List<Path> inModDirs, Path outModDir, List<String> opts)
            throws IOException, InterruptedException
    {
        AsciiArrayIOStream stdout = new AsciiArrayIOStream();
        AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        Path tmpDir = Files.createTempDirectory(null);
        String stdoutStr = null;
        String stderrStr = null;
        try
        {
            boolean res = FortranFrontEnd.run(DRIVER_CFG, inStrm, stdout, stderr, inModDirs, outModDir, tmpDir, opts,
                    inFilename);
            stdoutStr = Utils.collectIntoString(stdout.getAsInputStreamUnsafe());
            stderrStr = Utils.collectIntoString(stderr.getAsInputStreamUnsafe());
            // System.out.println("stdout:\n'" + stdoutStr + "'\n");
            // System.out.println("stderr:\n'" + stderrStr + "'\n");
            final Map<String, Path> outModFiles = new HashMap<String, Path>();
            Files.walk(outModDir).filter(Files::isRegularFile)
                    .forEach((Path f) -> outModFiles.put(f.getFileName().toString(), f));
            return new RunResult(res, stdoutStr, stderrStr, outModFiles);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    RunResult run(InputStream inStrm, Path inFilename, Path outModDir, List<Path> inModDirs)
            throws IOException, InterruptedException
    {
        return run(inStrm, inFilename, inModDirs, outModDir, Collections.emptyList());
    }

    static String removeTime(String in)
    {
        return in.replaceFirst("time=\".*\"", "time=\"\"");
    }

    public void testRun() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path NORMAL_IN_REF_FILEPATH = REF_DIR.resolve("normal_stdout.txt");
            final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
            RunResult rRes = run(Files.newInputStream(NORMAL_IN_FILEPATH), null, Collections.emptyList(),
                    tmpModOutDirDir, Collections.emptyList());
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals(collectIntoString(NORMAL_IN_REF_FILEPATH), removeTime(rRes.stdout));
            assertTrue(rRes.outModFiles.containsKey("m1.xmod"));
            assertEquals(collectIntoString(REF_XMOD), collectIntoString(rRes.outModFiles.get("m1.xmod")));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testRunWithFilenameReplacement() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path NORMAL_IN_REF_FILEPATH = REF_DIR.resolve("normal_rep_stdout.txt");
            final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
            RunResult rRes = run(Files.newInputStream(NORMAL_IN_FILEPATH), Paths.get("/tmp/normal.f90"),
                    Collections.emptyList(), tmpModOutDirDir, Collections.emptyList());
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals(collectIntoString(NORMAL_IN_REF_FILEPATH), removeTime(rRes.stdout));
            assertTrue(rRes.outModFiles.containsKey("m1.xmod"));
            assertEquals(collectIntoString(REF_XMOD), collectIntoString(rRes.outModFiles.get("m1.xmod")));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testStdInclude() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path STD_INCLUDE_IN_FILEPATH = IN_DIR.resolve("std_include.f90");
            final Path STD_INCLUDE_IN_REF_FILEPATH = REF_DIR.resolve("std_include_stdout.txt");
            final Path REF_XMOD = REF_DIR.resolve("std_include.xmod");
            RunResult rRes = run(Files.newInputStream(STD_INCLUDE_IN_FILEPATH), null, Collections.emptyList(),
                    tmpModOutDirDir, Collections.emptyList());
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            // assertEquals(collectIntoString(STD_INCLUDE_IN_REF_FILEPATH),
            // removeTime(rRes.stdout));
            assertTrue(rRes.outModFiles.containsKey("m_std.xmod"));

        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testError() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path ERR_IN_FILEPATH = IN_DIR.resolve("err.f90");
            final Path ERR_IN_REF_FILEPATH = REF_DIR.resolve("err_stderr.txt");
            RunResult rRes = run(Files.newInputStream(ERR_IN_FILEPATH), null, Collections.emptyList(), tmpModOutDirDir,
                    Collections.emptyList());
            assertFalse(rRes.res);
            assertEquals("", rRes.stdout);
            assertEquals(collectIntoString(ERR_IN_REF_FILEPATH), rRes.stderr);
            assertTrue(rRes.outModFiles.isEmpty());
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testErrorWithFilenameReplacement() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path ERR_IN_FILEPATH = IN_DIR.resolve("err.f90");
            final Path ERR_IN_REF_FILEPATH = REF_DIR.resolve("err_rep_stderr.txt");
            RunResult rRes = run(Files.newInputStream(ERR_IN_FILEPATH), Paths.get("/tmp/err.f90"),
                    Collections.emptyList(), tmpModOutDirDir, Collections.emptyList());
            assertFalse(rRes.res);
            assertEquals("", rRes.stdout);
            assertEquals(collectIntoString(ERR_IN_REF_FILEPATH), rRes.stderr);
            assertTrue(rRes.outModFiles.isEmpty());
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testXmodOnlyGeneration() throws IOException, InterruptedException
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
            RunResult rRes = run(Files.newInputStream(NORMAL_IN_FILEPATH), null, Collections.emptyList(),
                    tmpModOutDirDir, Arrays.asList("-module-compile"));
            assertTrue(rRes.res);
            assertTrue(rRes.outModFiles.isEmpty());
            assertEquals(collectIntoString(REF_XMOD), rRes.stdout);
            assertEquals("", rRes.stderr);
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

//    Not supported by current OMNI
//    public void testBlockXmodOutput() throws IOException, InterruptedException
//    {
//        Path tmpModOutDirDir = Files.createTempDirectory(null);
//        try
//        {
//            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
//            final Path NORMAL_IN_REF_FILEPATH = REF_DIR.resolve("normal_stdout.txt");
//            final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
//            // IN_DIR already contains m1.xmod
//            RunResult rRes = run(Files.newInputStream(NORMAL_IN_FILEPATH), null, Arrays.asList(IN_DIR), tmpModOutDirDir,
//                    Collections.emptyList());
//            assertTrue(rRes.res);
//            assertEquals("", rRes.stderr);
//            assertEquals(collectIntoString(NORMAL_IN_REF_FILEPATH), removeTime(rRes.stdout));
//            assertTrue(rRes.outModFiles.containsKey("m1.xmod"));
//            assertEquals(collectIntoString(REF_XMOD), collectIntoString(rRes.outModFiles.get("m1.xmod")));
//        } finally
//        {
//            if (tmpModOutDirDir != null)
//            {
//                Utils.removeDir(tmpModOutDirDir);
//            }
//        }
//    }
}
