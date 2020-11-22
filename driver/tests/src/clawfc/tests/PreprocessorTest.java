/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import clawfc.Driver;
import clawfc.Utils;

public class PreprocessorTest extends clawfc.tests.utils.DriverTestCase
{
    String readTxt(Path path) throws Exception
    {
        return new String(Files.readAllBytes(Paths.get(path.toString())), StandardCharsets.UTF_8);
    }

    String readTxtWithPPFilter(Path fileName) throws Exception
    {// Filters out preprocessor line markers
        String res;
        try (Scanner s = new Scanner(new File(fileName.toString())))
        {
            List<String> list = new ArrayList<String>();
            while (s.hasNextLine())
            {
                String line = s.nextLine();
                if (!line.startsWith("# "))
                {
                    list.add(line);
                }
            }
            res = String.join("\n", list) + "\n";
            return res;
        }
    }

    boolean equals(Path res, Path ref) throws Exception
    {
        String resTxt = readTxtWithPPFilter(res);
        String refTxt = readTxt(ref);
        if (!resTxt.equals(refTxt))
        {
            return false;
        }
        return true;
    }

    boolean equals(Path res, String refTxt) throws Exception
    {
        String resTxt = readTxtWithPPFilter(res);
        if (!resTxt.equals(refTxt))
        {
            return false;
        }
        return true;
    }

    public void testBasic() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("preprocessing/basic/input");
        final Path OUT_DIR = TMP_DIR;
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/basic/input/1.f90");
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "-O", OUT_DIR.toString(), INPUT_DIR.resolve("1.f90").toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testCLAWMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/claw_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/claw_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testOpenMPMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/openmp_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/openmp_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "--directive=openmp", "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testOpenACCMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/openacc_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/openacc_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "--directive=openacc", "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testAddMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/add_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/add_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "-D", "BLA", "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testIncludeDir() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/include_dir/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/include_dir/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        final Path incDir = RES_DIR.resolve("preprocessing/include_dir/input/inc");
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", TMP_DIR.toString(),
                "-O", OUT_DIR.toString(), "-I=" + incDir.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testSrcIncludeDirs() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("preprocessing/src_include_dirs/input");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/include_dir/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR, INT_DIR = TMP_DIR;
        final Path dir1 = INPUT_DIR, dir2 = INPUT_DIR.resolve("dir2"), dir3 = INPUT_DIR.resolve("dir2/dir3");
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--disable-mp", "--int-dir", INT_DIR.toString(),
                "-O", OUT_DIR.toString(), "-S", dir1.toString(), dir2.toString(), dir3.toString() };
        run(args);
        Path tmpDir1 = Paths.get(INT_DIR.resolve("include").toString() + dir1.toString());
        Path tmpDir2 = Paths.get(INT_DIR.resolve("include").toString() + dir2.toString());
        Path tmpDir3 = Paths.get(INT_DIR.resolve("include").toString() + dir3.toString());
        assertTrue(Utils.dirExists(tmpDir1));
        assertTrue(Utils.dirExists(tmpDir2));
        assertTrue(Utils.dirExists(tmpDir3));
        assertTrue(equals(tmpDir1.resolve("1.pp.f90"), dir1.resolve("1.f90")));
        assertTrue(equals(tmpDir2.resolve("2.pp.f90"), dir2.resolve("2.f90")));
        assertTrue(equals(tmpDir3.resolve("1.pp.f90"), dir3.resolve("1.f90")));
        assertTrue(equals(tmpDir3.resolve("3.pp.F95"), dir3.resolve("3.F95")));
    }

    public void testSkip() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/skip/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/skip/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--stop-pp", "--skip-pp", "--disable-mp", "--int-dir",
                TMP_DIR.toString(), "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = TMP_DIR.resolve("input/1.pp.f90");
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testMultiprocessing() throws Exception
    {
        final int N = 100;
        final Path OUT_DIR = TMP_DIR;
        final Path INPUT_TEMPLATE_FILEPATH = RES_DIR.resolve("preprocessing/multiprocessing/input/i.f90.template");
        final Path REF_TEMPLATE_FILEPATH = RES_DIR.resolve("preprocessing/multiprocessing/reference/i.f90.template");
        final Path INT_DIR = TMP_DIR.resolve("int");
        String INPUT_FILE_PATH_TEMPLATE = TMP_DIR.toString() + "/%s.f90";
        String inTemplate;
        try (InputStream inStrm = new FileInputStream(INPUT_TEMPLATE_FILEPATH.toString()))
        {
            inTemplate = Utils.collectIntoString(inStrm);
        }
        List<String> argsLst = new ArrayList<String>(Arrays.asList("--keep-int-files", "--stop-pp", "--int-dir",
                INT_DIR.toString(), "-O", OUT_DIR.toString()));
        for (int i = 0; i < N; ++i)
        {
            String inFilePath = String.format(INPUT_FILE_PATH_TEMPLATE, i);
            try (OutputStream outStrm = new FileOutputStream(inFilePath))
            {
                outStrm.write(String.format(inTemplate, i, i, i).getBytes(Charset.forName("UTF-8")));
            }
            argsLst.add(inFilePath);
        }
        String[] args = argsLst.stream().toArray(String[]::new);
        // ----------------------------
        run(args);
        // ----------------------------
        String refTemplate;
        try (InputStream inStrm = new FileInputStream(REF_TEMPLATE_FILEPATH.toString()))
        {
            refTemplate = Utils.collectIntoString(inStrm);
        }
        for (int i = 0; i < N; ++i)
        {
            final String refTxt = String.format(refTemplate, i, i, i);
            final Path resFilepath = INT_DIR.resolve(String.format("input/%s.pp.f90", i));
            assertTrue(equals(resFilepath, refTxt));
        }
    }

    public void testErrorLog() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/error_log/input/1.f90");
        final Path OUT_DIR = TMP_DIR, INT_DIR = TMP_DIR;
        String[] args = new String[] { DRIVER_PATH.toString(), "--keep-int-files", "--stop-pp", "--disable-mp",
                "--int-dir", INT_DIR.toString(), "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        ProcessBuilder pb = new ProcessBuilder(args);
        Process p = pb.start();
        final int retCode = p.waitFor();
        assertEquals(1, retCode);
        final Path errLogPath = INT_DIR.resolve("input/1.pp.f90.log");
        assertTrue(Utils.fileExists(errLogPath));
        String ppErrLog;
        try (InputStream inStrm = new FileInputStream(errLogPath.toString()))
        {
            ppErrLog = Utils.collectIntoString(inStrm);
        }
        if (Driver.cfg().defaultFortranCompilerType().equals("GNU"))
        {
            assertTrue(ppErrLog.contains("Fatal Error: not_found.inc: No such file or directory"));
        }
    }
}
