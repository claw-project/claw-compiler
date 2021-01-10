/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import static clawfc.PreprocessedFortranSourceData.getOutputFilePath;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

import clawfc.Configuration.FortranCompilerVendor;
import clawfc.Driver;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.SimplePathHashGenerator;

public class PreprocessorTest extends clawfc.tests.utils.DriverTestCase
{
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

    static Path outputPath(Path srcFilePath, Path outDir) throws NoSuchAlgorithmException
    {
        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        String fileName = srcFilePath.getFileName().toString();
        Path srcDirPath = srcFilePath.getParent();
        String srcDirHash = hashGen.generate(srcDirPath);
        Path outPath = getOutputFilePath(fileName, outDir, srcDirHash);
        return outPath;
    }

    public void testBasic() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("preprocessing/basic/input");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/basic/input/1.f90");
        final Path OUT_DIR = TMP_DIR;
        final Path inSrcFilePath = INPUT_DIR.resolve("1.f90");
        String[] args = new String[] { "--stop-pp", "-PO", OUT_DIR.toString(), "--disable-mp",
                inSrcFilePath.toString() };
        run(args);
        final Path resFilepath = outputPath(inSrcFilePath, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testCLAWMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/claw_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/claw_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--stop-pp", "-PO", OUT_DIR.toString(), "--disable-mp",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testOpenMPMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/openmp_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/openmp_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--stop-pp", "-PO", OUT_DIR.toString(), "--disable-mp", "--directive=openmp",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testOpenACCMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/openacc_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/openacc_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--stop-pp", "-PO", OUT_DIR.toString(), "--disable-mp", "--directive=openacc",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testAddMacro() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/add_macro/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/add_macro/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "--stop-pp", "-PO", OUT_DIR.toString(), "--disable-mp",
                INPUT_FILEPATH.toString(), "-D", "BLA" };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testIncludeDir() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/include_dir/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/include_dir/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        final Path incDir = RES_DIR.resolve("preprocessing/include_dir/input/inc");
        String[] args = new String[] { "-PO", OUT_DIR.toString(), "--stop-pp", "--disable-mp",
                "-I=" + incDir.toString(), INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testSrcIncludeDirs() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("preprocessing/src_include_dirs/input/input");
        final Path INC_DIR = RES_DIR.resolve("preprocessing/src_include_dirs/input/inc");
        final Path INPUT_FILEPATH = INPUT_DIR.resolve("blank.f90");
        final Path OUT_DIR = TMP_DIR;
        final Path dir1 = INC_DIR, dir2 = INC_DIR.resolve("dir2"), dir3 = INC_DIR.resolve("dir2/dir3");
        String[] args = new String[] { "-PO", OUT_DIR.toString(), INPUT_FILEPATH.toString(), "--stop-pp",
                "--disable-mp", "-S", dir1.toString(), dir2.toString(), dir3.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        final Path inc1Filepath = dir1.resolve("1.f90");
        final Path inc2Filepath = dir2.resolve("2.f90");
        final Path inc3Filepath = dir3.resolve("3.F95");
        final Path res1Filepath = outputPath(dir1.resolve("1.f90"), OUT_DIR);
        final Path res2Filepath = outputPath(dir2.resolve("2.f90"), OUT_DIR);
        final Path res3Filepath = outputPath(dir3.resolve("3.F95"), OUT_DIR);
        assertTrue(equals(INPUT_FILEPATH, resFilepath));
        assertTrue(equals(inc1Filepath, res1Filepath));
        assertTrue(equals(inc2Filepath, res2Filepath));
        assertTrue(equals(inc3Filepath, res3Filepath));
    }

    public void testSkip() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/skip/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("preprocessing/skip/reference/1.pp.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-PO", OUT_DIR.toString(), "--stop-pp", "--skip-pp", "--disable-mp",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertTrue(equals(resFilepath, REF_FILEPATH));
    }

    public void testMultiprocessing() throws Exception
    {
        final int N = 100;
        final Path OUT_DIR = TMP_DIR;
        final Path INPUT_TEMPLATE_FILEPATH = RES_DIR.resolve("preprocessing/multiprocessing/input/i.f90.template");
        final Path REF_TEMPLATE_FILEPATH = RES_DIR.resolve("preprocessing/multiprocessing/reference/i.f90.template");
        final Path IN_DIR = TMP_DIR.resolve("in");
        Files.createDirectory(IN_DIR);
        String INPUT_FILE_PATH_TEMPLATE = IN_DIR.toString() + "/%s.f90";
        String inTemplate = collectIntoString(INPUT_TEMPLATE_FILEPATH);
        List<String> argsLst = new ArrayList<String>(
                Arrays.asList("-PO", OUT_DIR.toString(), "--keep-int-files", "--stop-pp"));
        List<Path> inputFilePaths = new ArrayList<Path>(N);
        for (int i = 0; i < N; ++i)
        {
            Path inFilePath = Paths.get(sprintf(INPUT_FILE_PATH_TEMPLATE, i));
            inputFilePaths.add(inFilePath);
            try (AsciiArrayIOStream inFileTxt = new AsciiArrayIOStream(sprintf(inTemplate, i, i, i)))
            {
                saveToFile(inFileTxt.getAsInputStreamUnsafe(), inFilePath);
            }
            argsLst.add(inFilePath.toString());
        }
        String[] args = argsLst.stream().toArray(String[]::new);
        // ----------------------------
        run(args);
        // ----------------------------
        String refTemplate = collectIntoString(REF_TEMPLATE_FILEPATH);
        for (int i = 0; i < N; ++i)
        {
            final Path inputFilePath = inputFilePaths.get(i);
            final String refTxt = String.format(refTemplate, i, i, i);
            final Path resFilepath = outputPath(inputFilePath, OUT_DIR);
            assertTrue(equals(resFilepath, refTxt));
        }
    }

    public void testError() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("preprocessing/error/input/1.f90");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-PO", OUT_DIR.toString(), "--stop-pp", "--disable-mp",
                INPUT_FILEPATH.toString() };
        // ----------------------------
        Result res;
        Exception eCaught = null;
        try
        {
            res = run(args);
        } catch (Exception e)
        {
            eCaught = e;
        }
        assertTrue(eCaught != null);
        final String errMsg = eCaught.getMessage();
        assertTrue(errMsg.contains("Subprocess failed"));
        if (Driver.cfg().defaultFortranCompilerVendor() == FortranCompilerVendor.gnu)
        {
            assertTrue(errMsg.contains("Fatal Error: not_found.inc: No such file or directory"));
        }
    }
}
