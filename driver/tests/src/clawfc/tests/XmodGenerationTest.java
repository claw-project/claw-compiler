/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests;

import static clawfc.BuildInfo.XMOD_FILE_SEARCH_PATTERN;
import static clawfc.BuildInfo.createDirFileList;
import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.recreateDir;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

import clawfc.depscan.FortranSemanticException;

public class XmodGenerationTest extends clawfc.tests.utils.DriverTestCase
{
    FileTime touch(Path path) throws IOException
    {
        final FileTime ts = FileTime.from(Instant.now());
        Files.setLastModifiedTime(path, ts);
        return ts;
    }

    public void testInput() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/input_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/input_files/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        List<String> modNames = Arrays.asList("mod11.xmod", "mod12.xmod", "mod13.xmod");
        String[] args = new String[] { "--stop-xmod-gen", "--skip-pp", "--disable-mp", "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
        assertFalse(fileExists(OUT_MOD_DIR.resolve("mod_no_claw.mod")));
    }

    public void testOutputOption() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/input_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/input_files/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        List<String> modNames = Arrays.asList("mod11.xmod", "mod12.xmod", "mod13.xmod");
        String[] args = new String[] { "--gen-mod-files", "--skip-pp", "--disable-mp", "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
        assertFalse(fileExists(OUT_MOD_DIR.resolve("mod_no_claw.mod")));
    }

    public void testNoOutputDir() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/input_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/input_files/reference");
        final Path INT_DIR = TMP_DIR.resolve("int"), OUT_MOD_DIR = clawfc.FortranFrontEnd.intOutputDir(INT_DIR);
        List<String> modNames = Arrays.asList("mod11.xmod", "mod12.xmod", "mod13.xmod");
        String[] args = new String[] { "--stop-xmod-gen", "--keep-int-files", "--skip-pp", "--disable-mp", "--int-dir",
                INT_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
        assertFalse(fileExists(OUT_MOD_DIR.resolve("mod_no_claw.mod")));
    }

    public void testInclude() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/include_dir/input/p1.f90");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/include_dir/input/inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/include_dir/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods"), INT_DIR = TMP_DIR.resolve("int");
        List<String> modNames = Arrays.asList("mod1.xmod", "mod2.xmod");
        String[] args = new String[] { "--gen-mod-files", "--keep-int-files", "--skip-pp", "--disable-mp", "--int-dir",
                INT_DIR.toString(), "-S", INC_DIR.toString(), "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
    }

    public void testStdInclude() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/std_includes/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/std_includes/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        final String modName = "mod1.xmod";
        String[] args = new String[] { "--stop-xmod-gen", "--skip-pp", "--disable-mp", "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        final List<Path> outXmods = createDirFileList(OUT_MOD_DIR, XMOD_FILE_SEARCH_PATTERN);
        assertEquals(1, outXmods.size());
        final Path resFilePath = OUT_MOD_DIR.resolve(modName);
        assertEquals(resFilePath, outXmods.get(0));
        {
            final Path refFilePath = REF_MOD_DIR.resolve(modName);
            // Comparison impossible because of ffront's random hashes
            // assertTrue(this.equalsTxtFiles(resFilePath, refFilePath));
        }
    }

    public void testModInclude() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/xmod_include/input/p1.f90");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/inc");
        final Path MOD_INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/xmod_inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/xmod_include/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        List<String> modNames = Arrays.asList("mod1.xmod", "mod2.xmod");
        String[] args = new String[] { "--stop-xmod-gen", "--disable-mp", "-MI", MOD_INC_DIR.toString(), "-MO",
                OUT_MOD_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
    }

    public void testExistingModInclude() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/xmod_include/input/p1.f90");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/inc");
        final Path MOD_INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/xmod_inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/xmod_include/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        List<String> modNames = Arrays.asList("mod1.xmod", "mod2.xmod");
        String[] args = new String[] { "--stop-xmod-gen", "--disable-mp", "-MI", MOD_INC_DIR.toString(), "-MO",
                OUT_MOD_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
    }

    public void testCombinedInclude() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/xmod_include/input/p1.f90");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/inc");
        final Path MOD_INC_DIR = RES_DIR.resolve("xmod_generation/xmod_include/input/xmod_inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/xmod_include/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        List<String> modNames = Arrays.asList("mod1.xmod", "mod2.xmod");
        Files.createDirectory(OUT_MOD_DIR);
        String[] args = new String[] { INPUT_FILEPATH.toString(), "--stop-xmod-gen", "--disable-mp", "-J",
                OUT_MOD_DIR.toString(), "-J", MOD_INC_DIR.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
    }

    public void testForceTranslation() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/input_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/input_files/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods"), INT_DIR = TMP_DIR.resolve("int");
        List<String> modNames = Arrays.asList("mod11.xmod", "mod12.xmod", "mod13.xmod", "mod_no_claw.xmod");
        String[] args = new String[] { "--stop-xmod-gen", "--keep-int-files", "--skip-pp", "--disable-mp", "--force",
                "--int-dir", INT_DIR.toString(), "-MO", OUT_MOD_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }
    }

    public void testMultiprocessing() throws Exception
    {
        final int N = 100;
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/multiprocessing/input/p.f90");
        final Path INC_FILEPATH0 = RES_DIR.resolve("xmod_generation/multiprocessing/input/inc/i0.f90");
        final Path INC_TEMPLATE_FILEPATH = RES_DIR.resolve("xmod_generation/multiprocessing/input/inc/i.f90.template");
        final Path INPUT_DIR = TMP_DIR.resolve("input");
        final Path INPUT_INC_DIR = INPUT_DIR.resolve("inc");
        final Path REF_INDEP_XMOD_TEMPLATE_FILEPATH = RES_DIR
                .resolve("xmod_generation/multiprocessing/reference/m_indep_i.xmod.template");
        final Path REF_DEP_XMOD_TEMPLATE_FILEPATH = RES_DIR
                .resolve("xmod_generation/multiprocessing/reference/m_dep_next_i.xmod.template");
        final Path INT_DIR = TMP_DIR.resolve("int");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        final String INPUT_INC_FILE_PATH_TEMPLATE = INPUT_INC_DIR.toString() + "/%s.f90";
        final String incTemplate = collectIntoString(INC_TEMPLATE_FILEPATH);
        final String depXmodTemplate = collectIntoString(REF_DEP_XMOD_TEMPLATE_FILEPATH);
        final String indepXmodTemplate = collectIntoString(REF_INDEP_XMOD_TEMPLATE_FILEPATH);
        // Prepare input
        Files.createDirectories(INPUT_INC_DIR);
        Files.copy(INC_FILEPATH0, INPUT_INC_DIR.resolve("i0.f90"));
        for (int i = 1; i <= N; ++i)
        {
            Path incFilePath = Paths.get(sprintf(INPUT_INC_FILE_PATH_TEMPLATE, i));
            String incFileData = incTemplate.replace("{i}", String.valueOf(i));
            if (i < N)
            {
                incFileData = incFileData.replace("{use}", sprintf("use mod_dep_next_%s", i + 1));
            } else
            {
                incFileData = incFileData.replace("{use}", "");
            }
            try (PrintWriter p = new PrintWriter(Files.newOutputStream(incFilePath)))
            {
                p.print(incFileData);
            }
        }
        List<String> argsLst = new ArrayList<String>(
                Arrays.asList("--stop-xmod-gen", "--keep-int-files", "--skip-pp", "--int-dir", INT_DIR.toString(), "-S",
                        INPUT_INC_DIR.toString(), "-MO", OUT_MOD_DIR.toString(), INPUT_FILEPATH.toString()));
        String[] args = argsLst.stream().toArray(String[]::new);
        run(args);
        for (int i = 0; i < N; ++i)
        {
            String refDepXmod = depXmodTemplate.replace("{i}", String.valueOf(i)).replace("{i+1}",
                    String.valueOf(i + 1));
            String refIndepXmod = indepXmodTemplate.replace("{i}", String.valueOf(i)).replace("{i+1}",
                    String.valueOf(i + 1));
            String resDepXmod = collectIntoString(OUT_MOD_DIR.resolve(sprintf("mod_dep_next_%s.xmod", i)));
            String resIndepXmod = collectIntoString(OUT_MOD_DIR.resolve(sprintf("m_indep_%s.xmod", i)));
            assertEquals(refDepXmod, resDepXmod);
            assertEquals(refIndepXmod, resIndepXmod);
        }
    }

    static class VerifyRegeneration
    {
        public String[] regenArgs;
        final String[] xmodNames;
        final Path outModDir;
        final Path refModDir;
        public Map<String, FileTime> current;

        public VerifyRegeneration(String[] regenArgs, String[] xmodNames, Path outModDir, Path refModDir)
        {
            this.regenArgs = regenArgs;
            this.xmodNames = xmodNames;
            this.outModDir = outModDir;
            this.refModDir = refModDir;
        }

        public void updateCurrent() throws Exception
        {
            Map<String, FileTime> tsByName = new HashMap<String, FileTime>();
            for (String xmodName : xmodNames)
            {
                final Path resFilePath = outModDir.resolve(xmodName);
                FileTime ts = Files.getLastModifiedTime(resFilePath);
                tsByName.put(xmodName, ts);
            }
            current = tsByName;
        }

        public void verifyRegeneration(String... fileNames) throws Exception
        {
            run(regenArgs);
            verifyOutput();
            Map<String, FileTime> prev = current;
            updateCurrent();
            Set<String> fileNamesSet = new HashSet<String>();
            for (String xmodName : fileNames)
            {
                fileNamesSet.add(xmodName);
            }
            for (String xmodName : xmodNames)
            {
                FileTime prevTS = prev.get(xmodName);
                FileTime ts = current.get(xmodName);
                if (fileNamesSet.contains(xmodName))
                {
                    assertTrue(ts.compareTo(prevTS) > 0);
                } else
                {
                    assertEquals(prevTS, ts);
                }
            }
        }

        public void verifyOutput() throws Exception
        {
            for (String modName : xmodNames)
            {
                final Path resFilePath = outModDir.resolve(modName);
                final Path refFilePath = refModDir.resolve(modName);
                assertEqualsTxtFiles(refFilePath, resFilePath);
            }
        }
    }

    public void testRegeneration() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/regeneration/input/m.f90");
        final Path PP_INC_FILEPATH = RES_DIR.resolve("xmod_generation/regeneration/input/m.inc");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/regeneration/input/inc");
        final Path MOD_INC_FILEPATH1 = INC_DIR.resolve("mod1.f90");
        final Path MOD_INC_FILEPATH2 = INC_DIR.resolve("mod2.f90");
        final Path FTN_INC_FILEPATH = INC_DIR.resolve("mod2.inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/regeneration/reference");
        final Path OUT_XMOD_DIR = TMP_DIR.resolve("mods");
        final Path OUT_PP_SRC_DIR = TMP_DIR.resolve("pp");
        final Path OUT_BINFO_DIR = TMP_DIR.resolve("binfo");
        final String[] xmodNames = new String[] { "m.xmod", "mod1.xmod", "mod2.xmod" };
        Files.createDirectories(OUT_XMOD_DIR);
        class ArgsHolder
        {
            public String[] args;
        }
        final ArgsHolder argsHolder = new ArgsHolder();
        Callable<Void> verifyRegeneration = new Callable<Void>()
        {
            public Void call() throws Exception
            {
                recreateDir(OUT_XMOD_DIR);
                recreateDir(OUT_PP_SRC_DIR);
                recreateDir(OUT_BINFO_DIR);
                String[] regenArgs = argsHolder.args;
                VerifyRegeneration ts = new VerifyRegeneration(regenArgs, xmodNames, OUT_XMOD_DIR, REF_MOD_DIR);

                {// Generate buildinfo and xmod
                    run(regenArgs);
                    ts.verifyOutput();
                    ts.updateCurrent();
                }

                {// Source didn't change => no regeneration
                    ts.verifyRegeneration();
                }
                {// Source modified => regeneration
                    touch(INPUT_FILEPATH);
                    ts.verifyRegeneration("m.xmod");
                }
                {// Preprocessor-included source modified => regeneration
                    touch(PP_INC_FILEPATH);
                    ts.verifyRegeneration("m.xmod");
                }
                {// Included module source modified => regeneration
                    touch(MOD_INC_FILEPATH1);
                    ts.verifyRegeneration("m.xmod", "mod1.xmod");
                    touch(MOD_INC_FILEPATH2);
                    ts.verifyRegeneration("m.xmod", "mod1.xmod", "mod2.xmod");
                }
                {// Fortran-included source modified => regeneration
                    touch(FTN_INC_FILEPATH);
                    ts.verifyRegeneration("m.xmod", "mod1.xmod", "mod2.xmod");
                }
                {// Dependency xmod modified => regeneration
                    ts.current.put("mod2.xmod", touch(OUT_XMOD_DIR.resolve("mod2.xmod")));
                    ts.verifyRegeneration("m.xmod", "mod1.xmod");
                }
                return null;
            }
        };

        argsHolder.args = new String[] { "--stop-xmod-gen", "--disable-mp", "-SI", INC_DIR.toString(), "-MI",
                OUT_XMOD_DIR.toString(), "-MO", OUT_XMOD_DIR.toString(), INPUT_FILEPATH.toString() };
        verifyRegeneration.call();
        argsHolder.args = new String[] { "--stop-xmod-gen", "-SI", INC_DIR.toString(), "-MI", OUT_XMOD_DIR.toString(),
                "-MO", OUT_XMOD_DIR.toString(), INPUT_FILEPATH.toString() };
        verifyRegeneration.call();
        argsHolder.args = new String[] { "--stop-xmod-gen", "--disable-mp", "-SI", INC_DIR.toString(), "-MI",
                OUT_XMOD_DIR.toString(), "-MO", OUT_XMOD_DIR.toString(), "-BI", OUT_BINFO_DIR.toString(), "-BO",
                OUT_BINFO_DIR.toString(), "-PO", OUT_PP_SRC_DIR.toString(), INPUT_FILEPATH.toString() };
        verifyRegeneration.call();
    }

    public void testErrorMessage() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/error_msg/input/p1.f90");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        String[] args = new String[] { "--stop-xmod-gen", "--skip-pp", "--disable-mp", "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        final List<String> REF_UNIT_NAMES = Arrays.asList("p1", "m1", "m2", "m3");
        Result runRes = run(args, false);
        /*-TODO: This test only works in junit launch
        assertTrue(runRes.stderr.contains("SEVERE: Xmod generation: Error! Call to Omni frontend for m3 "));
        assertTrue(runRes.stderr.contains("Include stack:"));
        for (String unitName : REF_UNIT_NAMES)
        {
            assertTrue(runRes.stderr.contains(unitName));
        }*/
    }

    public void testStopOnFirstError() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/stop_on_first_error/input/p1.f90");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods");
        String[] args = new String[] { "--stop-xmod-gen", "--skip-pp", "--disable-mp", "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        final List<String> REF_UNIT_NAMES = Arrays.asList("p1", "m1", "m2", "m3");
        boolean exCaught = false;
        try
        {
            run(args, true);
        } catch (Exception e)
        {
            exCaught = true;
            final String errMsg = e.getMessage();
            // m2 and m3 could be processed, but are aborted because of the prior error in
            // m1
            assertTrue(errMsg.contains("failed: only 0 out of 4 successful"));
        }
        assertTrue(exCaught);
    }

    public void testNoDep() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("xmod_generation/no_dep/input/m1.f90");
        final Path INC_DIR = RES_DIR.resolve("xmod_generation/no_dep/input/inc");
        final Path MOD_INC_DIR = RES_DIR.resolve("xmod_generation/no_dep/input/mod_inc");
        final Path REF_MOD_DIR = RES_DIR.resolve("xmod_generation/no_dep/reference");
        final Path OUT_MOD_DIR = TMP_DIR.resolve("mods"), INT_DIR = TMP_DIR.resolve("int");
        final Path OUT_DIR = TMP_DIR.resolve("out");
        List<String> modNames = Arrays.asList("m1.xmod");
        try
        {
            String[] args = new String[] { "--no-dep", "--gen-mod-files", "-O", OUT_DIR.toString(), "--disable-mp",
                    "--int-dir", INT_DIR.toString(), "-MO", OUT_MOD_DIR.toString(), INPUT_FILEPATH.toString() };
            run(args);
        } catch (FortranSemanticException e)
        {
            final String errMsg = e.getMessage();
            assertTrue(errMsg.contains("Module \"m2\" used by MODULE m1"));
            assertTrue(errMsg.contains("is not defined in any file under given search path"));
        }
        {// Source available, but it is forbidden to use it because of "--no-dep"
            String[] args = new String[] { "--no-dep", "--gen-mod-files", "-O", OUT_DIR.toString(), "--disable-mp",
                    "-SI", INC_DIR.toString(), "--int-dir", INT_DIR.toString(), "-MO", OUT_MOD_DIR.toString(),
                    INPUT_FILEPATH.toString() };
            Result res = run(args, false);
            assertTrue(res.exception != null);
            /*
             * assertTrue(res.stderr.
             * contains("Xmod generation: Error! Up to date xmod file for dependency module m2"
             * )); assertTrue(res.stderr.contains("is not available"));
             */
        }
        String[] args = new String[] { "--no-dep", "--gen-mod-files", "-O", OUT_DIR.toString(), "--disable-mp", "-MI",
                MOD_INC_DIR.toString(), "--int-dir", INT_DIR.toString(), "-MO", OUT_MOD_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : modNames)
        {
            Path resFilePath = OUT_MOD_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            this.equalsTxtFiles(resFilePath, refFilePath);
        }

    }
}
