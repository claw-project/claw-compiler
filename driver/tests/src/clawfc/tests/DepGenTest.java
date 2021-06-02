/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.fileExists;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import clawfc.Configuration;

public class DepGenTest extends clawfc.tests.utils.DriverTestCase
{
    final String INPUT_BASENAME = "main.f90";
    final String INPUT_BASENAME2 = "other.f90";
    final Path INPUT_DIR = RES_DIR.resolve("depgen/input");
    final Path INC_DIR = INPUT_DIR.resolve("inc");
    final Path MOD_SRC_INC_DIR = INPUT_DIR;
    final Path XMOD_INC_DIR = INPUT_DIR.resolve("xmod");
    final Path INPUT_FILEPATH1 = INPUT_DIR.resolve(INPUT_BASENAME);
    final Path INPUT_FILEPATH2 = INPUT_DIR.resolve(INPUT_BASENAME2);

    void verifyDepFile(Path depFilePath, Path outDir) throws Exception
    {
        assertTrue(fileExists(depFilePath));
        final String depRule = collectIntoString(depFilePath);
        String[] parts = depRule.split(":");
        assertEquals(parts[0], outDir.resolve(INPUT_BASENAME).toString());
        String[] depFilesArr = parts[1].replace("\\\n", "\n").split("\\r?\\n");
        Set<String> depFiles = new HashSet<String>();
        for (String s : depFilesArr)
        {
            s = s.trim();
            if (!s.isEmpty())
            {
                depFiles.add(s);
            }
        }
        String[] expDepFiles = new String[] { "inc/1.inc", "inc/2.inc", "xmod/xmod_only_mod.xmod", "main.f90",
                "other.f90" };

        for (String expDepFilename : expDepFiles)
        {
            String expDepPath = INPUT_DIR.resolve(expDepFilename).toString();
            assertTrue(depFiles.contains(expDepPath));
        }
        clawfc.Configuration DRIVER_CFG = new Configuration();
        final String stdXmod = DRIVER_CFG.omniDefaultStdXmodDir().resolve("iso_c_binding.xmod").toString();
        assertTrue(depFiles.contains(stdXmod));
    }

    public void testSingleFile() throws Exception
    {
        final Path OUT_DIR = TMP_DIR.resolve("out");
        {
            final Path DEP_FILEPATH = OUT_DIR.resolve(INPUT_BASENAME + ".d");
            String[] args = new String[] { "-MD", "-I", INC_DIR.toString(), "-SI", MOD_SRC_INC_DIR.toString(), "-MI",
                    XMOD_INC_DIR.toString(), "--stop-xast-gen", "--disable-mp", "-o",
                    OUT_DIR.resolve(INPUT_BASENAME).toString(), INPUT_FILEPATH1.toString() };
            run(args);
            verifyDepFile(DEP_FILEPATH, OUT_DIR);
            Files.delete(DEP_FILEPATH);
        }
        {
            final Path DEP_FILEPATH = OUT_DIR.resolve(INPUT_BASENAME + ".d");
            String[] args = new String[] { "-MD", DEP_FILEPATH.toString(), "-I", INC_DIR.toString(), "-SI",
                    MOD_SRC_INC_DIR.toString(), "-MI", XMOD_INC_DIR.toString(), "--stop-xast-gen", "--disable-mp", "-o",
                    OUT_DIR.resolve(INPUT_BASENAME).toString(), INPUT_FILEPATH1.toString() };
            run(args);
            verifyDepFile(DEP_FILEPATH, OUT_DIR);
            Files.delete(DEP_FILEPATH);
        }
        {
            final Path DEP_FILEPATH = OUT_DIR.resolve(INPUT_BASENAME + ".d");
            String[] args = new String[] { "-MD", DEP_FILEPATH.toString(), "-I", INC_DIR.toString(), "-SI",
                    MOD_SRC_INC_DIR.toString(), "-MI", XMOD_INC_DIR.toString(), "--stop-xast-gen", "--disable-mp", "-O",
                    OUT_DIR.toString(), INPUT_FILEPATH1.toString() };
            run(args);
            verifyDepFile(DEP_FILEPATH, OUT_DIR);
            Files.delete(DEP_FILEPATH);
        }
    }

    public void testMultipleFiles() throws Exception
    {
        final Path OUT_DIR = TMP_DIR.resolve("out");
        {
            final Path DEP_FILEPATH = OUT_DIR.resolve(INPUT_BASENAME + ".d");
            final Path DEP_FILEPATH2 = OUT_DIR.resolve(INPUT_BASENAME2 + ".d");
            String[] args = new String[] { "-MD", "-I", INC_DIR.toString(), "-SI", MOD_SRC_INC_DIR.toString(), "-MI",
                    XMOD_INC_DIR.toString(), "--stop-xast-gen", "--disable-mp", "-O", OUT_DIR.toString(),
                    INPUT_FILEPATH1.toString(), INPUT_FILEPATH2.toString() };
            run(args);
            verifyDepFile(DEP_FILEPATH, OUT_DIR);
            assertTrue(Files.exists(DEP_FILEPATH2));
        }
    }
}
