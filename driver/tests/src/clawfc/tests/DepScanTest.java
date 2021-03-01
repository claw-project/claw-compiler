/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests;

import static clawfc.FortranFileProgramUnitInfoData.getOutputFilePath;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;
import static clawfc.Utils.toInputStream;
import static clawfc.Utils.touch;
import static java.util.Collections.emptyList;

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
import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranFileProgramUnitInfoDeserializer;
import clawfc.depscan.FortranFileProgramUnitInfoSerializer;
import clawfc.depscan.FortranProgramUnitInfo;
import clawfc.depscan.FortranStatementPosition;
import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.SimplePathHashGenerator;

public class DepScanTest extends clawfc.tests.utils.DriverTestCase
{
    public static FortranFileProgramUnitInfo loadInfo(Path path) throws Exception
    {
        FortranFileProgramUnitInfoDeserializer deserializer = new FortranFileProgramUnitInfoDeserializer(true);
        try (InputStream inStrm = Files.newInputStream(path))
        {
            return deserializer.deserialize(inStrm);
        }
    }

    public static FortranFileProgramUnitInfo loadInfo(String txt) throws Exception
    {
        try (InputStream inStrm = toInputStream(txt))
        {
            return loadInfo(inStrm);
        }
    }

    public static FortranFileProgramUnitInfo loadInfo(InputStream inStrm) throws Exception
    {
        FortranFileProgramUnitInfoDeserializer deserializer = new FortranFileProgramUnitInfoDeserializer(true);
        return deserializer.deserialize(inStrm);
    }

    static void addPath(Path infoPath, Path src) throws Exception
    {
        FortranFileProgramUnitInfo info = loadInfo(infoPath);
        info.setSrcFilePath(src);
        FortranFileProgramUnitInfoSerializer serializer = new FortranFileProgramUnitInfoSerializer();
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
        FortranFileProgramUnitInfo refInfo;
        try (AsciiArrayIOStream buf = new AsciiArrayIOStream(refStr))
        {
            refInfo = loadInfo(buf.getAsInputStreamUnsafe());
        }
        FortranFileProgramUnitInfo resInfo = loadInfo(filePath);
        assertEquals(refInfo, resInfo);
    }

    static FortranProgramUnitInfo ModInfo(clawfc.depscan.FortranStatementPosition pos,
            List<clawfc.depscan.FortranStatementPosition> useModules, boolean usesClaw)
    {
        return new FortranProgramUnitInfo(FortranProgramUnitType.MODULE, pos, useModules, usesClaw);
    }

    static FortranProgramUnitInfo ProgInfo(clawfc.depscan.FortranStatementPosition pos,
            List<clawfc.depscan.FortranStatementPosition> useModules, boolean usesClaw)
    {
        return new FortranProgramUnitInfo(FortranProgramUnitType.PROGRAM, pos, useModules, usesClaw);
    }

    public static FortranStatementPosition Pos(String name, int startCharIdx, int endCharIdx, int startLineIdx,
            int endLineIdx)
    {
        return new FortranStatementPosition(name, startCharIdx, endCharIdx, startLineIdx, endLineIdx);
    }

    public void testInputScan() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/input_files/input/1.f90");
        final FortranFileProgramUnitInfo REF_INFO;
        {
            final List<FortranProgramUnitInfo> units = Arrays
                    .asList(ModInfo(Pos("mod11", 0, 36, 0, 3), emptyList(), true),
                            ModInfo(Pos("mod12", 38, 67, 4, 6), emptyList(), false),
                            ModInfo(Pos("mod13", 69, 112, 7, 10), Arrays.asList(Pos("mod12", 86, 95, 8, 9)), false),
                            ProgInfo(Pos("p1", 114, 167, 11, 15),
                                    Arrays.asList(Pos("mod12", 129, 138, 12, 13), Pos("mod13", 143, 152, 13, 14)),
                                    false));
            REF_INFO = new FortranFileProgramUnitInfo(units, INPUT_FILEPATH, INPUT_FILEPATH, emptyList());
        }
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-BO", OUT_DIR.toString(), "--stop-depscan", "--skip-pp", "--disable-mp",
                INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertEquals(REF_INFO, loadInfo(resFilepath));
    }

    public void testInputScanWithIgnore() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/ignore/input/1.f90");
        final FortranFileProgramUnitInfo REF_INFO;
        {
            final List<FortranProgramUnitInfo> units = Arrays.asList(
                    ModInfo(Pos("mod11", 0, 29, 0, 2), emptyList(), false),
                    ModInfo(Pos("mod12", 31, 74, 3, 6), Arrays.asList(Pos("mod11", 48, 57, 4, 5)), false),
                    ModInfo(Pos("mod13", 76, 136, 7, 10), emptyList(), false),
                    ProgInfo(Pos("p1", 138, 247, 11, 17), Arrays.asList(Pos("mod12", 153, 162, 12, 13)), true));
            REF_INFO = new FortranFileProgramUnitInfo(units, INPUT_FILEPATH, null, emptyList());
        }
        final Path OUT_DIR = TMP_DIR;
        final Path OUT_PP_DIR = TMP_DIR.resolve("pp");
        String[] args = new String[] { "-BO", OUT_DIR.toString(), "-PO", OUT_PP_DIR.toString(), "--stop-depscan",
                "--disable-mp", INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        final FortranFileProgramUnitInfo resInfo = loadInfo(resFilepath);
        resInfo.setPPSrcFilePath(null);
        assertEquals(REF_INFO, resInfo);
    }

    public void testInputWithIncludeScan() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("depscan/input_with_include/input/1.f90");
        final Path OUT_DIR = TMP_DIR;
        final FortranFileProgramUnitInfo REF_INFO;
        {
            final List<FortranProgramUnitInfo> units = Arrays
                    .asList(ModInfo(Pos("mod11", 1, 37, 1, 4), emptyList(), true),
                            ModInfo(Pos("mod12", 39, 68, 5, 7), emptyList(), false),
                            ModInfo(Pos("mod13", 70, 113, 8, 11), Arrays.asList(Pos("mod12", 87, 96, 9, 10)), false),
                            ProgInfo(Pos("p1", 115, 168, 12, 16),
                                    Arrays.asList(Pos("mod12", 130, 139, 13, 14), Pos("mod13", 144, 153, 14, 15)),
                                    false));
            SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
            final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
            final Path ppFilePath = OUT_DIR.resolve(srcDirHash + "_1.pp.f90");
            final List<Path> incFiles = Arrays.asList(RES_DIR.resolve("depscan/input_with_include/input/1.inc"),
                    RES_DIR.resolve("depscan/input_with_include/input/2.inc"));
            REF_INFO = new FortranFileProgramUnitInfo(units, INPUT_FILEPATH, ppFilePath, incFiles);
        }
        String[] args = new String[] { "-BO", OUT_DIR.toString(), "-PO", OUT_DIR.toString(), "--stop-depscan",
                "--disable-mp", INPUT_FILEPATH.toString() };
        run(args);
        final Path resFilepath = outputPath(INPUT_FILEPATH, OUT_DIR);
        assertEquals(REF_INFO, loadInfo(resFilepath));
    }

    public void testIncludeScan() throws Exception
    {
        final Path INPUT_DIR = RES_DIR.resolve("depscan/include_dir/input");
        final Path INC_DIR = INPUT_DIR.resolve("inc");
        final Path[] INPUT_FILES = new Path[] { INPUT_DIR.resolve("blank.f90"), INC_DIR.resolve("1.f90"),
                INC_DIR.resolve("2.f90") };
        final int N = INPUT_FILES.length;
        final FortranFileProgramUnitInfo[] REF_INFO = new FortranFileProgramUnitInfo[] {
                new FortranFileProgramUnitInfo(emptyList(), INPUT_FILES[0], INPUT_FILES[0], emptyList()),
                new FortranFileProgramUnitInfo(Arrays.asList(ModInfo(Pos("mod11", 0, 36, 0, 3), emptyList(), true),
                        ModInfo(Pos("mod12", 38, 67, 4, 6), emptyList(), false),
                        ModInfo(Pos("mod13", 69, 112, 7, 10), Arrays.asList(Pos("mod12", 86, 95, 8, 9)), false),
                        ProgInfo(Pos("p1", 114, 167, 11, 15),
                                Arrays.asList(Pos("mod12", 129, 138, 12, 13), Pos("mod13", 143, 152, 13, 14)), false)),
                        INPUT_FILES[1], INPUT_FILES[1], emptyList()),
                new FortranFileProgramUnitInfo(Arrays.asList(
                        ModInfo(Pos("mod21", 0, 50, 0, 4), Arrays.asList(Pos("mod13", 17, 26, 1, 2)), true),
                        ModInfo(Pos("mod22", 52, 81, 5, 7), emptyList(), false),
                        ModInfo(Pos("mod23", 83, 126, 8, 11), Arrays.asList(Pos("mod22", 100, 109, 9, 10)), false),
                        ProgInfo(Pos("p2", 128, 181, 12, 16),
                                Arrays.asList(Pos("mod22", 143, 152, 13, 14), Pos("mod23", 157, 166, 14, 15)), false)),
                        INPUT_FILES[2], INPUT_FILES[2], emptyList()) };
        final Path OUT_DIR = TMP_DIR;
        String[] args = new String[] { "-BO", OUT_DIR.toString(), INPUT_FILES[0].toString(), "--stop-depscan",
                "--skip-pp", "--disable-mp", "-SI", INC_DIR.toString() };
        run(args);
        for (int i = 0; i < N; ++i)
        {
            final Path resFilepath = outputPath(INPUT_FILES[i], OUT_DIR);
            assertEquals(REF_INFO[i], loadInfo(resFilepath));
        }
    }

    public void testOutput() throws Exception
    {
        final Path INPUT_FILEPATH1 = RES_DIR.resolve("depscan/output/input/1.f90");
        final Path INPUT_FILEPATH2 = RES_DIR.resolve("depscan/output/input/2.f90");
        final Path OUT_DIR = TMP_DIR.resolve("out"), INT_DIR = TMP_DIR.resolve("int");
        final FortranFileProgramUnitInfo[] REF_INFO = new FortranFileProgramUnitInfo[] {
                new FortranFileProgramUnitInfo(Arrays.asList(ModInfo(Pos("mod11", 0, 36, 0, 3), emptyList(), true),
                        ModInfo(Pos("mod12", 38, 67, 4, 6), emptyList(), false),
                        ModInfo(Pos("mod13", 69, 112, 7, 10), Arrays.asList(Pos("mod12", 86, 95, 8, 9)), false),
                        ProgInfo(Pos("p1", 114, 167, 11, 15),
                                Arrays.asList(Pos("mod12", 129, 138, 12, 13), Pos("mod13", 143, 152, 13, 14)), false)),
                        INPUT_FILEPATH1, INPUT_FILEPATH1, emptyList()),
                new FortranFileProgramUnitInfo(Arrays.asList(
                        ModInfo(Pos("mod21", 0, 50, 0, 4), Arrays.asList(Pos("mod13", 17, 26, 1, 2)), true),
                        ModInfo(Pos("mod22", 52, 81, 5, 7), emptyList(), false),
                        ModInfo(Pos("mod23", 83, 126, 8, 11), Arrays.asList(Pos("mod22", 100, 109, 9, 10)), false),
                        ProgInfo(Pos("p2", 128, 181, 12, 16),
                                Arrays.asList(Pos("mod22", 143, 152, 13, 14), Pos("mod23", 157, 166, 14, 15)), false)),
                        INPUT_FILEPATH2, INPUT_FILEPATH2, emptyList()) };
        String[] args = new String[] { "--gen-buildinfo-files", "--skip-pp", "--disable-mp", "-BO", OUT_DIR.toString(),
                INPUT_FILEPATH1.toString(), INPUT_FILEPATH2.toString() };
        run(args);
        final Path resFilepath1 = outputPath(INPUT_FILEPATH1, OUT_DIR, false);
        final Path resFilepath2 = outputPath(INPUT_FILEPATH2, OUT_DIR, false);
        assertEquals(REF_INFO[0], loadInfo(resFilepath1));
        assertEquals(REF_INFO[1], loadInfo(resFilepath2));
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
        final String INPUT_FILE_PATH_TEMPLATE = IN_DIR.toString() + "/%s.f90";
        final String inTemplate = Utils.collectIntoString(INPUT_TEMPLATE_FILEPATH);
        final FortranFileProgramUnitInfo[] REF_INFO = new FortranFileProgramUnitInfo[N];
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
            REF_INFO[i] = new FortranFileProgramUnitInfo(
                    Arrays.asList(ModInfo(Pos(sprintf("mod%s1", i), 0, 36, 0, 3), emptyList(), true),
                            ModInfo(Pos(sprintf("mod%s2", i), 38, 67, 4, 6), emptyList(), false),
                            ModInfo(Pos(sprintf("mod%s3", i), 69, 112, 7, 10),
                                    Arrays.asList(Pos(sprintf("mod%s2", i), 86, 95, 8, 9)), false),
                            ProgInfo(Pos(sprintf("p%s", i), 114, 167, 11, 15),
                                    Arrays.asList(Pos(sprintf("mod%s2", i), 129, 138, 12, 13),
                                            Pos(sprintf("mod%s3", i), 143, 152, 13, 14)),
                                    false)),
                    inFilePath, inFilePath, emptyList());
        }
        String[] args = argsLst.stream().toArray(String[]::new);
        // ----------------------------
        run(args);
        // ----------------------------
        for (int i = 0; i < N; ++i)
        {
            Path resPath = outputPath(inputFilePaths.get(i), OUT_DIR);
            assertEquals(REF_INFO[i], loadInfo(resPath));
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
        final Path REF_PP_FILEPATH = RES_DIR.resolve("depscan/regeneration/reference/p.pp.f90");

        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
        final Path resPPFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.pp.f90");
        final Path resBinfoFilepath = OUT_BINFO_DIR.resolve("p.f90.fif");
        final FortranFileProgramUnitInfo REF_INFO = new FortranFileProgramUnitInfo(
                Arrays.asList(ProgInfo(Pos("p", 0, 30, 0, 3), emptyList(), true)), INPUT_FILEPATH, resPPFilepath,
                Arrays.asList(FTN_INC_FILEPATH, PP_INC_FILEPATH));
        Callable<Void> verifyOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_PP_FILEPATH, resPPFilepath);
                assertEquals(REF_INFO, loadInfo(resBinfoFilepath));
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
        final Path REF_PP_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/reference/p.pp.f90");

        final Path INC_INPUT_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/m.f90");
        final Path INC_PP_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/pp2.inc");
        final Path INC_FTN_INC_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/input/inc/f2.inc");
        final Path INC_DIR = RES_DIR.resolve("depscan/inc_regeneration/input/inc");
        final Path REF_INC_PP_FILEPATH = RES_DIR.resolve("depscan/inc_regeneration/reference/m.pp.f90");

        SimplePathHashGenerator hashGen = new SimplePathHashGenerator();
        final String srcDirHash = hashGen.generate(INPUT_FILEPATH.getParent());
        final Path resPPFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.pp.f90");
        final Path resBinfoFilepath = OUT_BINFO_DIR.resolve(srcDirHash + "_p.f90.fif");

        final FortranFileProgramUnitInfo PRG_REF_INFO = new FortranFileProgramUnitInfo(
                Arrays.asList(ProgInfo(Pos("p", 0, 30, 0, 3), emptyList(), true)), INPUT_FILEPATH, resPPFilepath,
                Arrays.asList(PP_INC_FILEPATH, FTN_INC_FILEPATH));

        final String incSrcDirHash = hashGen.generate(INC_DIR);
        final Path resIncPPFilepath = OUT_BINFO_DIR.resolve(incSrcDirHash + "_m.pp.f90");
        final Path resIncBinfoFilepath = OUT_BINFO_DIR.resolve(incSrcDirHash + "_m.f90.fif");
        final FortranFileProgramUnitInfo MOD_REF_INFO = new FortranFileProgramUnitInfo(
                Arrays.asList(ModInfo(Pos("m", 0, 28, 0, 3), emptyList(), true)), INC_INPUT_FILEPATH, resIncPPFilepath,
                Arrays.asList(INC_PP_INC_FILEPATH, INC_FTN_INC_FILEPATH));// Preprocessor include has higher priority

        Callable<Void> verifyOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_PP_FILEPATH, resPPFilepath);
                assertEquals(PRG_REF_INFO, loadInfo(resBinfoFilepath));
                return null;
            }
        };

        Callable<Void> verifyIncOutput = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                assertEqualsTxtFiles(REF_INC_PP_FILEPATH, resIncPPFilepath);
                assertEquals(MOD_REF_INFO, loadInfo(resIncBinfoFilepath));
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
