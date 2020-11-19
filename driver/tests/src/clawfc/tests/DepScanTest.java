/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

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

import clawfc.Driver;
import clawfc.Utils;
import clawfc.depscan.FortranFileSummary;
import clawfc.depscan.FortranFileSummaryDeserializer;

public class DepScanTest extends clawfc.tests.utils.DriverTestCase
{
    static String readTxt(Path path) throws Exception
    {
        return new String(Files.readAllBytes(Paths.get(path.toString())), StandardCharsets.UTF_8);
    }

    static boolean equalsTxtFiles(Path res, Path ref) throws Exception
    {
        String refTxt = readTxt(ref);
        return txtFileEqualsTxt(res, refTxt);
    }

    static boolean txtFileEqualsTxt(Path res, String refTxt) throws Exception
    {
        String resTxt = readTxt(res);
        if (!resTxt.equals(refTxt))
        {
            return false;
        }
        return true;
    }

    static FortranFileSummary loadInfo(Path path) throws Exception
    {
        FortranFileSummaryDeserializer deserializer = new FortranFileSummaryDeserializer(true);
        try (InputStream inStrm = Files.newInputStream(path))
        {
            return deserializer.deserialize(inStrm);
        }
    }

    public void testInputScan() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/input_files/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("depscan/input_files/reference/1.f90.fif");
        final Path OUT_DIR = TMP_DIR, INT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--skip-pp", "--disable-mp", "--int-dir", INT_DIR.toString(),
                "-O", OUT_DIR.toString(), INPUT_FILEPATH.toString() };
        Driver.run(args);
        Path resFilepath = TMP_DIR.resolve("input/1.f90.fif");
        assertTrue(equalsTxtFiles(resFilepath, REF_FILEPATH));
    }

    public void testIncludeScan() throws Exception
    {
        final Path INC_DIR = RES_DIR.resolve("depscan/include_dir/input");
        final Path REF_DIR = RES_DIR.resolve("depscan/include_dir/reference");
        final Path INPUT_FILEPATH1 = INC_DIR.resolve("1.f90");
        final Path REF_FILEPATH1 = REF_DIR.resolve("1.f90.fif");
        final Path INPUT_FILEPATH2 = INC_DIR.resolve("2.f90");
        final Path REF_FILEPATH2 = REF_DIR.resolve("2.f90.fif");
        final Path OUT_DIR = TMP_DIR, INT_DIR = TMP_DIR;
        String[] args = new String[] { "--keep-int-files", "--skip-pp", "--disable-mp", "--int-dir", INT_DIR.toString(),
                "-O", OUT_DIR.toString(), "-S", INC_DIR.toString() };
        Driver.run(args);
        Path RES_DIR = Paths.get(INT_DIR.resolve("include").toString() + INC_DIR.toString());
        Path resFilepath1 = RES_DIR.resolve("1.f90.fif");
        Path resFilepath2 = RES_DIR.resolve("2.f90.fif");
        assertTrue(equalsTxtFiles(resFilepath1, REF_FILEPATH1));
        assertTrue(equalsTxtFiles(resFilepath2, REF_FILEPATH2));
    }

    public void testOutput() throws Exception
    {
        final Path INPUT_FILEPATH1 = RES_DIR.resolve("depscan/output/input/1.f90");
        final Path INPUT_FILEPATH2 = RES_DIR.resolve("depscan/output/input/2.f90");
        final Path REF_FILEPATH1 = RES_DIR.resolve("depscan/output/reference/1.f90.fif");
        final Path REF_FILEPATH2 = RES_DIR.resolve("depscan/output/reference/2.f90.fif");
        final Path OUT_DIR = TMP_DIR.resolve("out"), INT_DIR = TMP_DIR.resolve("int");
        String[] args = new String[] { "--gen-buildinfo-files", "--keep-int-files", "--skip-pp", "--disable-mp",
                "--int-dir", INT_DIR.toString(), "-O", OUT_DIR.toString(), INPUT_FILEPATH1.toString(),
                INPUT_FILEPATH2.toString() };
        Driver.run(args);
        Path resTmpFilepath1 = INT_DIR.resolve("input/1.f90.fif");
        Path resTmpFilepath2 = INT_DIR.resolve("input/2.f90.fif");
        assertTrue(equalsTxtFiles(resTmpFilepath1, REF_FILEPATH1));
        assertTrue(equalsTxtFiles(resTmpFilepath2, REF_FILEPATH2));
        Path resFilepath1 = OUT_DIR.resolve("1.f90.fif");
        Path resFilepath2 = OUT_DIR.resolve("2.f90.fif");
        FortranFileSummary ref1 = loadInfo(REF_FILEPATH1);
        ref1.setFilePath(INPUT_FILEPATH1);
        FortranFileSummary ref2 = loadInfo(REF_FILEPATH2);
        ref2.setFilePath(INPUT_FILEPATH2);
        FortranFileSummary res1 = loadInfo(resFilepath1);
        FortranFileSummary res2 = loadInfo(resFilepath2);
        assertEquals(ref1, res1);
        assertEquals(ref2, res2);
    }

    public void testMultiprocessing() throws Exception
    {
        final int N = 10;
        final Path OUT_DIR = TMP_DIR;
        final Path INPUT_TEMPLATE_FILEPATH = RES_DIR.resolve("depscan/multiprocessing/input/i.f90.template");
        final Path REF_TEMPLATE_FILEPATH = RES_DIR.resolve("depscan/multiprocessing/reference/i.f90.fif.template");
        final Path INT_DIR = TMP_DIR.resolve("int");
        String INPUT_FILE_PATH_TEMPLATE = TMP_DIR.toString() + "/%s.f90";
        String inTemplate;
        try (InputStream inStrm = new FileInputStream(INPUT_TEMPLATE_FILEPATH.toString()))
        {
            inTemplate = Utils.collectIntoString(inStrm);
        }
        List<String> argsLst = new ArrayList<String>(Arrays.asList("--keep-int-files", "--skip-pp", "--int-dir",
                INT_DIR.toString(), "-O", OUT_DIR.toString()));
        for (int i = 0; i < N; ++i)
        {
            String inFilePath = String.format(INPUT_FILE_PATH_TEMPLATE, i);
            try (OutputStream outStrm = new FileOutputStream(inFilePath))
            {
                outStrm.write(inTemplate.replace("{i}", String.valueOf(i)).getBytes(Charset.forName("UTF-8")));
            }
            argsLst.add(inFilePath);
        }
        String[] args = argsLst.stream().toArray(String[]::new);
        // ----------------------------
        Driver.run(args);
        // ----------------------------
        String refTemplate;
        try (InputStream inStrm = new FileInputStream(REF_TEMPLATE_FILEPATH.toString()))
        {
            refTemplate = Utils.collectIntoString(inStrm);
        }
        for (int i = 0; i < N; ++i)
        {
            final String refTxt = refTemplate.replace("{i}", String.valueOf(i));
            final Path resFilepath = INT_DIR.resolve(String.format("input/%s.f90.fif", i));
            assertTrue(txtFileEqualsTxt(resFilepath, refTxt));
        }
    }
}
