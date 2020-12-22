/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import static clawfc.FortranFileBuildInfoData.getOutputFilePath;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;
import static clawfc.Utils.touch;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;

import clawfc.Utils;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranFileBuildInfoDeserializer;
import clawfc.depscan.FortranFileBuildInfoSerializer;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.SimplePathHashGenerator;

public class DepScanTest extends clawfc.tests.utils.DriverTestCase
{
    public static FortranFileBuildInfo loadInfo(Path path) throws Exception
    {
        FortranFileBuildInfoDeserializer deserializer = new FortranFileBuildInfoDeserializer(true);
        try (InputStream inStrm = Files.newInputStream(path))
        {
            return deserializer.deserialize(inStrm);
        }
    }

    public static FortranFileBuildInfo loadInfo(String txt) throws Exception
    {
        try (InputStream inStrm = clawfc.depscan.Utils.toInputStream(txt))
        {
            return loadInfo(inStrm);
        }
    }

    public static FortranFileBuildInfo loadInfo(InputStream inStrm) throws Exception
    {
        FortranFileBuildInfoDeserializer deserializer = new FortranFileBuildInfoDeserializer(true);
        return deserializer.deserialize(inStrm);
    }

    static void addPath(Path infoPath, Path src) throws Exception
    {
        FortranFileBuildInfo info = loadInfo(infoPath);
        info.setSrcFilePath(src);
        FortranFileBuildInfoSerializer serializer = new FortranFileBuildInfoSerializer();
        try (OutputStream outStrm = Files.newOutputStream(infoPath))
        {
            serializer.serialize(info, outStrm);
        }
    }

    static Path outputPath(Path srcFilePath, Path outDir) throws NoSuchAlgorithmException
    {
        return outputPath(srcFilePath, outDir, true);
    }

    static Path outputPath(Path srcFilePath, Path outDir, boolean addHash) throws NoSuchAlgorithmException
    {
        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        String fileName = srcFilePath.getFileName().toString();
        Path srcDirPath = srcFilePath.getParent();
        String srcDirHash = addHash ? hashGen.generate(srcDirPath) : null;
        Path outPath = getOutputFilePath(fileName, outDir, srcDirHash);
        return outPath;
    }

    void assertEqualsBuildInfo(Path resDirPath, Path refTemplatePath, Path filePath) throws Exception
    {
        final String refStr = collectIntoString(refTemplatePath).replace("{res_dir}", resDirPath.toString());
        assertEqualsBuildInfo(refStr, filePath);
    }

    void assertEqualsBuildInfo(String refStr, Path filePath) throws Exception
    {
        FortranFileBuildInfo refInfo;
        try (AsciiArrayIOStream buf = new AsciiArrayIOStream(refStr))
        {
            refInfo = loadInfo(buf.getAsInputStreamUnsafe());
        }
        FortranFileBuildInfo resInfo = loadInfo(filePath);
        assertEquals(refInfo, resInfo);
    }

    public void testInputScan() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/input_files/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("depscan/input_files/reference/1.f90.fif.template");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-BO", OUT_DIR.toString(), "--stop-depscan", "--skip-pp", "--disable-mp",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertEqualsBuildInfo(RES_DIR, REF_FILEPATH, resFilepath);
    }

    public void testInputWithIncludeScan() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/input_with_include/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("depscan/input_with_include/reference/1.f90.fif.template");
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-BO", OUT_DIR.toString(), "-PO", OUT_DIR.toString(), "--stop-depscan",
                "--disable-mp", INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
        final String refStr = collectIntoString(REF_FILEPATH).replace("{res_dir}", RES_DIR.toString())
                .replace("{out_dir}", OUT_DIR.toString()).replace("{hash}", srcDirHash);
        assertEqualsBuildInfo(refStr, resFilepath);
    }

    public void testIncludeScan() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("depscan/include_dir/input");
        final Path INC_DIR = INPUT_DIR.resolve("inc");
        final Path REF_DIR = RES_DIR.resolve("depscan/include_dir/reference");
        final Path[] INPUT_FILES = new Path[] { INPUT_DIR.resolve("blank.f90"), INC_DIR.resolve("1.f90"),
                INC_DIR.resolve("2.f90") };
        final Path[] REF_FILES = new Path[] { REF_DIR.resolve("blank.f90.fif.template"),
                REF_DIR.resolve("1.f90.fif.template"), REF_DIR.resolve("2.f90.fif.template") };
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-BO", OUT_DIR.toString(), INPUT_FILES[0].toString(), "--stop-depscan",
                "--skip-pp", "--disable-mp", "-SI", INC_DIR.toString() };
        run(args);
        final int N = INPUT_FILES.length;
        for (int i = 0; i < N; ++i)
        {
            final Path resFilepath = outputPath(INPUT_FILES[i], OUT_DIR);
            assertEqualsBuildInfo(RES_DIR, REF_FILES[i], resFilepath);
        }
    }

    public void testOutput() throws Exception
    {
        final Path INPUT_FILEPATH1 = RES_DIR.resolve("depscan/output/input/1.f90");
        final Path INPUT_FILEPATH2 = RES_DIR.resolve("depscan/output/input/2.f90");
        final Path REF_FILEPATH1 = RES_DIR.resolve("depscan/output/reference/1.f90.fif.template");
        final Path REF_FILEPATH2 = RES_DIR.resolve("depscan/output/reference/2.f90.fif.template");
        final Path OUT_DIR = TMP_DIR.resolve("out"), INT_DIR = TMP_DIR.resolve("int");
        String[] args = new String[] { "--gen-buildinfo-files", "--skip-pp", "--disable-mp", "-BO", OUT_DIR.toString(),
                INPUT_FILEPATH1.toString(), INPUT_FILEPATH2.toString() };
        run(args);
        final Path resFilepath1 = outputPath(INPUT_FILEPATH1, OUT_DIR, false);
        final Path resFilepath2 = outputPath(INPUT_FILEPATH2, OUT_DIR, false);
        assertEqualsBuildInfo(RES_DIR, REF_FILEPATH1, resFilepath1);
        assertEqualsBuildInfo(RES_DIR, REF_FILEPATH2, resFilepath2);
    }

    public void testOutputNameClash() throws Exception
    {// This does not work as intended due to earlier check on input files
        final Path INPUT_FILEPATH1 = RES_DIR.resolve("depscan/output_names_clash/input/in1/1.f90");
        final Path INPUT_FILEPATH2 = RES_DIR.resolve("depscan/output_names_clash/input/in2/1.f90");
        final Path OUT_DIR = TMP_DIR.resolve("out"), INT_DIR = TMP_DIR.resolve("int");
        String[] args = new String[] { "--gen-buildinfo-files", "--skip-pp", "--disable-mp", "-BO", OUT_DIR.toString(),
                INPUT_FILEPATH1.toString(), INPUT_FILEPATH2.toString() };
        boolean exceptionCaught = false;
        try
        {
            run(args);
        } catch (Exception e)
        {
            assertTrue(e.getMessage().contains("Input files cannot have identical names"));
            exceptionCaught = true;
        }
        assertTrue(exceptionCaught);
    }

    public void testMultiprocessing() throws Exception
    {
        final int N = 10;
        final Path IN_DIR = TMP_DIR.resolve("in");
        final Path OUT_DIR = TMP_DIR.resolve("out");
        final Path INPUT_TEMPLATE_FILEPATH = RES_DIR.resolve("depscan/multiprocessing/input/i.f90.template");
        final Path REF_TEMPLATE_FILEPATH = RES_DIR.resolve("depscan/multiprocessing/reference/i.f90.fif.template");
        final String INPUT_FILE_PATH_TEMPLATE = IN_DIR.toString() + "/%s.f90";
        final String inTemplate = Utils.collectIntoString(INPUT_TEMPLATE_FILEPATH);
        List<String> argsLst = new ArrayList<String>(
                Arrays.asList("--stop-depscan", "--skip-pp", "-BO", OUT_DIR.toString()));
        List<Path> inputFilePaths = new ArrayList<Path>(N);
        Files.createDirectories(IN_DIR);
        for (int i = 0; i < N; ++i)
        {
            Path inFilePath = Paths.get(sprintf(INPUT_FILE_PATH_TEMPLATE, i));
            inputFilePaths.add(inFilePath);
            try (AsciiArrayIOStream inFileTxt = new AsciiArrayIOStream(inTemplate.replace("{i}", String.valueOf(i))))
            {
                saveToFile(inFileTxt.getAsInputStreamUnsafe(), inFilePath);
            }
            argsLst.add(inFilePath.toString());
        }
        String[] args = argsLst.stream().toArray(String[]::new);
        // ----------------------------
        run(args);
        // ----------------------------
        final String refTemplate = Utils.collectIntoString(REF_TEMPLATE_FILEPATH);
        final String IN_DIR_STRING = IN_DIR.toString();
        for (int i = 0; i < N; ++i)
        {
            Path resPath = outputPath(inputFilePaths.get(i), OUT_DIR);
            final String refStr = refTemplate.replace("{i}", String.valueOf(i)).replace("{in_dir}", IN_DIR_STRING);
            assertEqualsBuildInfo(refStr, resPath);
        }
    }

    public void testCLAWDetection() throws Exception
    {
        final Path INPUT_FILEPATH1 = RES_DIR.resolve("depscan/claw_detection/input/no_claw.f90").normalize();
        final Path INPUT_FILEPATH2 = RES_DIR.resolve("depscan/claw_detection/input/in_module.f90").normalize();
        final Path INPUT_FILEPATH3 = RES_DIR.resolve("depscan/claw_detection/input/in_program.f90").normalize();
        final Path INPUT_FILEPATH4 = RES_DIR.resolve("depscan/claw_detection/input/in_inc.f90").normalize();
        String[] args = new String[] { "--disable-mp", "--print-claw-files", INPUT_FILEPATH1.toString(),
                INPUT_FILEPATH2.toString(), INPUT_FILEPATH3.toString(), INPUT_FILEPATH4.toString() };
        Result res = run(args);
        String ref = INPUT_FILEPATH2.toString() + "\n" + INPUT_FILEPATH3.toString() + "\n" + INPUT_FILEPATH4 + "\n";
        assertEquals(ref, res.stdout);
    }

    public void testRegeneration() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/regeneration/input/p.f90");
        final Path PP_INC_FILEPATH = RES_DIR.resolve("depscan/regeneration/input/f.inc");
        final Path FTN_INC_FILEPATH = RES_DIR.resolve("depscan/regeneration/input/pp.inc");
        final Path OUT_BINFO_DIR = TMP_DIR.resolve("binfo");
        final Path REF_BINFO_PATH = RES_DIR.resolve("depscan/regeneration/reference/p.f90.fif.template");
        final Path REF_PP_FILEPATH = RES_DIR.resolve("depscan/regeneration/reference/p.pp.f90");

        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
        final Path resPPFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.pp.f90");
        final Path resBinfoFilepath = OUT_BINFO_DIR.resolve("p.f90.fif");
        final String refBinfoStr = collectIntoString(REF_BINFO_PATH).replace("{res_dir}", RES_DIR.toString())
                .replace("{hash}", srcDirHash).replace("{out_dir}", OUT_BINFO_DIR.toString());

        Callable<Void> verifyOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_PP_FILEPATH, resPPFilepath);
                assertEqualsBuildInfo(refBinfoStr, resBinfoFilepath);
                return null;
            }
        };
        final String[] genArgs = new String[] { "--gen-buildinfo-files", "--disable-mp", "-BO",
                OUT_BINFO_DIR.toString(), "-PO", OUT_BINFO_DIR.toString(), INPUT_FILEPATH.toString() };
        final String[] regenArgs = new String[] { "--gen-buildinfo-files", "--disable-mp", "-BO",
                OUT_BINFO_DIR.toString(), "-BI", OUT_BINFO_DIR.toString(), "-PO", OUT_BINFO_DIR.toString(),
                INPUT_FILEPATH.toString() };
        {// Generate buildinfo
            run(genArgs);
            verifyOutput.call();
        }
        class Timestamps
        {
            public FileTime pp;
            public FileTime binfo;
        }
        ;
        Timestamps prevTS = new Timestamps();
        Callable<Void> verifyRegeneration = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                run(regenArgs);
                verifyOutput.call();
                FileTime ppTs = Files.getLastModifiedTime(resPPFilepath);
                FileTime binfoTs = Files.getLastModifiedTime(resBinfoFilepath);
                assertTrue("Preprocessed source was not regenerated", prevTS.pp.compareTo(ppTs) < 0);
                assertTrue("Buildinfo was not regenerated", prevTS.binfo.compareTo(binfoTs) < 0);
                prevTS.pp = ppTs;
                prevTS.binfo = binfoTs;
                return null;
            }
        };
        prevTS.pp = Files.getLastModifiedTime(resPPFilepath);
        prevTS.binfo = Files.getLastModifiedTime(resBinfoFilepath);
        {// Nothing changed => no regeneration
            run(regenArgs);
            verifyOutput.call();
            // Output files should be untouched
            FileTime ppTs = Files.getLastModifiedTime(resPPFilepath);
            FileTime binfoTs = Files.getLastModifiedTime(resBinfoFilepath);
            assertEquals("Preprocessed source was regenerated", prevTS.pp, ppTs);
            assertEquals("Buildinfo was regenerated", prevTS.binfo, binfoTs);
        }
        // Source modified => regeneration
        touch(INPUT_FILEPATH);
        verifyRegeneration.call();
        // Fortran-included Source modified => regeneration
        touch(FTN_INC_FILEPATH);
        verifyRegeneration.call();
        // Preprocessor-included Source modified => regeneration
        touch(PP_INC_FILEPATH);
        verifyRegeneration.call();
        // Preprocessed source modified => buildinfo outdated => regeneration
        touch(resPPFilepath);
        verifyRegeneration.call();
        // Buildinfo removed => no reference to (existing) preprocessed source =>
        // regeneration
        Files.delete(resBinfoFilepath);
        verifyRegeneration.call();
    }

    public void testIncRegeneration() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/p.f90");
        final Path PP_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/pp.inc");
        final Path FTN_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/f.inc");
        final Path OUT_BINFO_DIR = TMP_DIR.resolve("binfo");
        final Path REF_BINFO_PATH = RES_DIR.resolve("depscan/inc_regeneration/reference/p.f90.fif.template");
        final Path REF_PP_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/reference/p.pp.f90");

        final Path INC_INPUT_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/m.f90");
        final Path INC_PP_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/pp2.inc");
        final Path INC_FTN_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/f2.inc");
        final Path INC_DIR = RES_DIR.resolve("depscan/inc_regeneration/input/inc");
        final Path REF_INC_BINFO_PATH = RES_DIR.resolve("depscan/inc_regeneration/reference/m.f90.fif.template");
        final Path REF_INC_PP_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/reference/m.pp.f90");

        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
        final Path resPPFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.pp.f90");
        final Path resBinfoFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.f90.fif");
        final String refBinfoStr = collectIntoString(REF_BINFO_PATH).replace("{res_dir}", RES_DIR.toString())
                .replace("{hash}", srcDirHash).replace("{out_dir}", OUT_BINFO_DIR.toString());

        final String incSrcDirHash = hashGen.generate(INC_DIR);
        final Path resIncPPFilepath = OUT_BINFO_DIR.resolve(incSrcDirHash + "_m.pp.f90");
        final Path resIncBinfoFilepath = OUT_BINFO_DIR.resolve(incSrcDirHash + "_m.f90.fif");
        final String refIncBinfoStr = collectIntoString(REF_INC_BINFO_PATH).replace("{res_dir}", RES_DIR.toString())
                .replace("{hash}", incSrcDirHash).replace("{out_dir}", OUT_BINFO_DIR.toString());

        Callable<Void> verifyOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_PP_FILEPATH, resPPFilepath);
                assertEqualsBuildInfo(refBinfoStr, resBinfoFilepath);
                return null;
            }
        };

        Callable<Void> verifyIncOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_INC_PP_FILEPATH, resIncPPFilepath);
                assertEqualsBuildInfo(refIncBinfoStr, resIncBinfoFilepath);
                return null;
            }
        };
        final String[] genArgs = new String[] { "-SI", INC_DIR.toString(), "--stop-depscan", "--disable-mp", "-BO",
                OUT_BINFO_DIR.toString(), "-PO", OUT_BINFO_DIR.toString(), INPUT_FILEPATH.toString() };
        final String[] regenArgs = new String[] { "-SI", INC_DIR.toString(), "--stop-depscan", "--disable-mp", "-BO",
                OUT_BINFO_DIR.toString(), "-BI", OUT_BINFO_DIR.toString(), "-PO", OUT_BINFO_DIR.toString(),
                INPUT_FILEPATH.toString() };
        {// Generate buildinfo
            run(genArgs);
            verifyOutput.call();
        }
        class Timestamps
        {
            public FileTime pp;
            public FileTime binfo;
        }
        ;
        Timestamps prevTS = new Timestamps();
        Timestamps prevIncTS = new Timestamps();
        Callable<Void> verifyRegeneration = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                run(regenArgs);
                verifyOutput.call();
                FileTime ppTs = Files.getLastModifiedTime(resPPFilepath);
                FileTime binfoTs = Files.getLastModifiedTime(resBinfoFilepath);
                assertTrue("Preprocessed source was not regenerated", prevTS.pp.compareTo(ppTs) < 0);
                assertTrue("Buildinfo was not regenerated", prevTS.binfo.compareTo(binfoTs) < 0);
                prevTS.pp = ppTs;
                prevTS.binfo = binfoTs;
                // Verify inc output is unchanged
                FileTime ppIncTs = Files.getLastModifiedTime(resIncPPFilepath);
                FileTime binfoIncTs = Files.getLastModifiedTime(resIncBinfoFilepath);
                assertEquals(prevIncTS.pp, ppIncTs);
                assertEquals(prevIncTS.binfo, binfoIncTs);
                return null;
            }
        };
        Callable<Void> verifyIncRegeneration = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                run(regenArgs);
                verifyOutput.call();
                FileTime ppIncTs = Files.getLastModifiedTime(resIncPPFilepath);
                FileTime binfoIncTs = Files.getLastModifiedTime(resIncBinfoFilepath);
                assertTrue("Preprocessed source was not regenerated", prevIncTS.pp.compareTo(ppIncTs) < 0);
                assertTrue("Buildinfo was not regenerated", prevIncTS.binfo.compareTo(binfoIncTs) < 0);
                prevIncTS.pp = ppIncTs;
                prevIncTS.binfo = binfoIncTs;
                // Verify src output is unchanged
                FileTime ppTs = Files.getLastModifiedTime(resPPFilepath);
                FileTime binfoTs = Files.getLastModifiedTime(resBinfoFilepath);
                assertEquals(prevTS.pp, ppTs);
                assertEquals(prevTS.binfo, binfoTs);
                return null;
            }
        };
        prevTS.pp = Files.getLastModifiedTime(resPPFilepath);
        prevTS.binfo = Files.getLastModifiedTime(resBinfoFilepath);
        prevIncTS.pp = Files.getLastModifiedTime(resIncPPFilepath);
        prevIncTS.binfo = Files.getLastModifiedTime(resIncBinfoFilepath);
        {// Nothing changed => no regeneration
            run(regenArgs);
            verifyOutput.call();
            verifyIncOutput.call();
            // Output files should be untouched
            FileTime ppTs = Files.getLastModifiedTime(resPPFilepath);
            FileTime binfoTs = Files.getLastModifiedTime(resBinfoFilepath);
            assertEquals("Preprocessed source was regenerated", prevTS.pp, ppTs);
            assertEquals("Buildinfo was regenerated", prevTS.binfo, binfoTs);
            FileTime ppIncTs = Files.getLastModifiedTime(resIncPPFilepath);
            FileTime binfoIncTs = Files.getLastModifiedTime(resIncBinfoFilepath);
            assertEquals("Preprocessed inc source was regenerated", prevIncTS.pp, ppIncTs);
            assertEquals("Inc buildinfo was regenerated", prevIncTS.binfo, binfoIncTs);
        }
        // Source modified => regeneration
        touch(INPUT_FILEPATH);
        verifyRegeneration.call();
        // Fortran-included Source modified => regeneration
        touch(FTN_INC_FILEPATH);
        verifyRegeneration.call();
        // Preprocessor-included Source modified => regeneration
        touch(PP_INC_FILEPATH);
        verifyRegeneration.call();
        // Preprocessed source modified => buildinfo outdated => regeneration
        touch(resPPFilepath);
        verifyRegeneration.call();
        // Buildinfo removed => no reference to (existing) preprocessed source =>
        // regeneration
        Files.delete(resBinfoFilepath);
        verifyRegeneration.call();
        // ----------------------------

        // Source modified => regeneration
        touch(INC_INPUT_FILEPATH);
        verifyIncRegeneration.call();
        // Fortran-included Source modified => regeneration
        touch(INC_FTN_INC_FILEPATH);
        verifyIncRegeneration.call();
        // Preprocessor-included Source modified => regeneration
        touch(INC_PP_INC_FILEPATH);
        verifyIncRegeneration.call();
        // Preprocessed source modified => buildinfo outdated => regeneration
        touch(resIncPPFilepath);
        verifyIncRegeneration.call();
        // Buildinfo removed => no reference to (existing) preprocessed source =>
        // regeneration
        Files.delete(resIncBinfoFilepath);
        verifyIncRegeneration.call();
    }
}
