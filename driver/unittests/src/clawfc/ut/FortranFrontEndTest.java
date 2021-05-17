/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

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

        public RunResult(boolean res, String stdout, String stderr)
        {
            this.res = res;
            this.stdout = stdout;
            this.stderr = stderr;
        }
    }

    RunResult run(AsciiArrayIOStream inStrm, Path inFilename, List<Path> inModDirs, List<String> opts, Path outFilePath)
            throws Exception
    {
        AsciiArrayIOStream stdout = new AsciiArrayIOStream();
        AsciiArrayIOStream stderr = new AsciiArrayIOStream();
        Path tmpDir = Files.createTempDirectory(null);
        String stdoutStr = null;
        String stderrStr = null;
        List<Path> incXmods = new ArrayList<Path>();
        for (Path inModDirPath : inModDirs)
        {
            for (File f : inModDirPath.toFile().listFiles())
            {
                String filename = f.getName();
                if (filename.endsWith((".xmod")))
                {
                    incXmods.add(inModDirPath.resolve(filename));
                }
            }
        }
        try
        {
            List<String> opts2 = new ArrayList<String>(opts);
            opts2.addAll(Arrays.asList("--in-memory-mode", "-no-time"));
            boolean res = FortranFrontEnd.run(DRIVER_CFG, inStrm, outFilePath, stderr, incXmods, tmpDir, opts2,
                    inFilename, null, null);

            stdoutStr = Utils.collectIntoString(stdout.getAsInputStreamUnsafe());
            stderrStr = Utils.collectIntoString(stderr.getAsInputStreamUnsafe());
            // System.out.println("stdout:\n'" + stdoutStr + "'\n");
            // System.out.println("stderr:\n'" + stderrStr + "'\n");
            return new RunResult(res, stdoutStr, stderrStr);
        } finally
        {
            if (tmpDir != null)
            {
                Utils.removeDir(tmpDir);
            }
        }
    }

    RunResult run(AsciiArrayIOStream inStrm, Path inFilename, List<Path> inModDirs, Path outFilePath) throws Exception
    {
        return run(inStrm, inFilename, inModDirs, Collections.emptyList(), outFilePath);
    }

    public void testRunXmod() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
            final Path outXmodPath = tmpModOutDirDir.resolve("m1.xmod");
            RunResult rRes = run(new AsciiArrayIOStream(NORMAL_IN_FILEPATH), null, Collections.emptyList(),
                    Arrays.asList("-module-compile"), outXmodPath);
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals("", rRes.stdout);
            assertTrue(Files.exists(outXmodPath));
            assertEquals(collectIntoString(REF_XMOD), collectIntoString(outXmodPath));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testRunXast() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path NORMAL_IN_REF_FILEPATH = REF_DIR.resolve("normal_stdout.txt");
            final Path outXastPath = tmpModOutDirDir.resolve("m1.xml");
            RunResult rRes = run(new AsciiArrayIOStream(NORMAL_IN_FILEPATH), null, Collections.emptyList(),
                    outXastPath);
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals("", rRes.stdout);
            assertTrue(Files.exists(outXastPath));
            assertEquals(collectIntoString(NORMAL_IN_REF_FILEPATH), collectIntoString(outXastPath));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testRunWithFilenameReplacement() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path NORMAL_IN_FILEPATH = IN_DIR.resolve("normal.f90");
            final Path NORMAL_IN_REF_FILEPATH = REF_DIR.resolve("normal_stdout.txt");
            final Path outXastPath = tmpModOutDirDir.resolve("m1.xml");
            RunResult rRes = run(new AsciiArrayIOStream(NORMAL_IN_FILEPATH), NORMAL_IN_FILEPATH,
                    Collections.emptyList(), outXastPath);
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals("", rRes.stdout);
            assertTrue(Files.exists(outXastPath));
            assertEquals(
                    collectIntoString(NORMAL_IN_REF_FILEPATH).replace("&lt;stdin&gt;", NORMAL_IN_FILEPATH.toString()),
                    collectIntoString(outXastPath));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testStdInclude() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path STD_INCLUDE_IN_FILEPATH = IN_DIR.resolve("std_include.f90");
            final Path REF_XMOD = REF_DIR.resolve("std_include.xmod");
            final Path outXmodPath = tmpModOutDirDir.resolve("std_include.xmod");
            RunResult rRes = run(new AsciiArrayIOStream(STD_INCLUDE_IN_FILEPATH), null, Collections.emptyList(),
                    Arrays.asList("-module-compile"), outXmodPath);
            assertTrue(rRes.res);
            assertEquals("", rRes.stderr);
            assertEquals("", rRes.stdout);
            assertTrue(Files.exists(outXmodPath));
            assertEquals(collectIntoString(REF_XMOD), collectIntoString(outXmodPath));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testError() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path ERR_IN_FILEPATH = IN_DIR.resolve("err.f90");
            final Path ERR_IN_REF_FILEPATH = REF_DIR.resolve("err_stderr.txt");
            final Path outXastPath = tmpModOutDirDir.resolve("out.xml");
            RunResult rRes = run(new AsciiArrayIOStream(ERR_IN_FILEPATH), null, Collections.emptyList(),
                    Collections.emptyList(), outXastPath);
            assertFalse(rRes.res);
            assertEquals("", rRes.stdout);
            assertTrue(rRes.stderr.startsWith(collectIntoString(ERR_IN_REF_FILEPATH)));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testErrorWithFilenameReplacement() throws Exception
    {
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        try
        {
            final Path ERR_IN_FILEPATH = IN_DIR.resolve("err.f90");
            final Path ERR_IN_REF_FILEPATH = REF_DIR.resolve("err_stderr.txt");
            final Path outXastPath = tmpModOutDirDir.resolve("out.xml");
            RunResult rRes = run(new AsciiArrayIOStream(ERR_IN_FILEPATH), ERR_IN_FILEPATH, Collections.emptyList(),
                    Collections.emptyList(), outXastPath);
            assertFalse(rRes.res);
            assertEquals("", rRes.stdout);
            assertTrue(rRes.stderr
                    .startsWith(collectIntoString(ERR_IN_REF_FILEPATH).replace("<stdin>", ERR_IN_FILEPATH.toString())));
        } finally
        {
            if (tmpModOutDirDir != null)
            {
                Utils.removeDir(tmpModOutDirDir);
            }
        }
    }

    public void testBigInput() throws Exception
    {
        final Path REF_XMOD = REF_DIR.resolve("m1.xmod");
        final int N_KB = 100;
        final String bigInput;
        {
            final String line = String.join("", Collections.nCopies(64, " ")) + "\n";
            final String kbStr = String.join("", Collections.nCopies(16, line)) + "\n";
            bigInput = String.join("", Collections.nCopies(N_KB, kbStr)) + "module m1;end;";
        }
        Path tmpModOutDirDir = Files.createTempDirectory(null);
        final Path outXmodPath = tmpModOutDirDir.resolve("out.xml");
        try
        {
            RunResult rRes = run(new AsciiArrayIOStream(bigInput), null, Collections.emptyList(),
                    Arrays.asList("-module-compile"), outXmodPath);

            assertTrue(rRes.res);
            assertEquals(collectIntoString(REF_XMOD), collectIntoString(outXmodPath));
            assertEquals("", rRes.stderr);
            assertEquals("", rRes.stdout);
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
