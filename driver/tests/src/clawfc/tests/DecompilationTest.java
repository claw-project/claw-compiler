/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.tests;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.fileExists;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;

public class DecompilationTest extends clawfc.tests.utils.DriverTestCase
{
    public void testInput() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("decompilation/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("decompilation/reference");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        List<String> outSrcFilenames = Arrays.asList("mod11.f90", "mod12.f90", "mod13.f90", "p1.f90");
        String[] args = new String[] { "--debug", "--add-paren", "--stop-dec", "--disable-mp", "-TSO",
                OUT_SRC_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String filename : outSrcFilenames)
        {
            Path resFilePath = OUT_SRC_DIR.resolve(filename);
            Path refFilePath = REF_MOD_DIR.resolve(filename);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
        assertFalse(fileExists(OUT_SRC_DIR.resolve("mod_no_claw.f90")));
    }

    public void testForceInput() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("decompilation/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("decompilation/reference");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        List<String> outSrcFilenames = Arrays.asList("mod11.f90", "mod12.f90", "mod13.f90", "p1.f90",
                "mod_no_claw.f90");
        String[] args = new String[] { "--force", "--add-paren", "--stop-dec", "--disable-mp", "-TSO",
                OUT_SRC_DIR.toString(), INPUT_FILEPATH.toString() };
        run(args);
        for (String filename : outSrcFilenames)
        {
            Path resFilePath = OUT_SRC_DIR.resolve(filename);
            Path refFilePath = REF_MOD_DIR.resolve(filename);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
    }

    public void testMultiprocessing() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("decompilation/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("decompilation/reference");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        List<String> outSrcFilenames = Arrays.asList("mod11.f90", "mod12.f90", "mod13.f90", "p1.f90",
                "mod_no_claw.f90");
        String[] args = new String[] { "--force", "--add-paren", "--stop-dec", "-TSO", OUT_SRC_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String filename : outSrcFilenames)
        {
            Path resFilePath = OUT_SRC_DIR.resolve(filename);
            Path refFilePath = REF_MOD_DIR.resolve(filename);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
    }
}
