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

public class TranslationTest extends clawfc.tests.utils.DriverTestCase
{
    public void testInput() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("translation/without_src_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("translation/without_src_files/reference");
        final Path OUT_XAST_DIR = TMP_DIR.resolve("xast");
        List<String> xastNames = Arrays.asList("mod11.xast", "mod12.xast", "mod13.xast", "p1.xast");
        String[] args = new String[] { "--debug", "--stop-trans", "--disable-mp", "-TXO", OUT_XAST_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : xastNames)
        {
            Path resFilePath = OUT_XAST_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
        assertFalse(fileExists(OUT_XAST_DIR.resolve("mod_no_claw.xast")));
    }

    public void testInputWithSourceFile() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("translation/with_src_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("translation/with_src_files/reference");
        final Path OUT_XAST_DIR = TMP_DIR.resolve("xast");
        List<String> xastNames = Arrays.asList("mod11.xast", "mod12.xast", "mod13.xast", "p1.xast");
        String[] args = new String[] { "--stop-trans", "--disable-mp", "--skip-pp", "-TXO", OUT_XAST_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : xastNames)
        {
            Path resFilePath = OUT_XAST_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath))).replace("{filename}",
                    INPUT_FILEPATH.toString());
            assertEquals(refStr, resStr);
        }
        assertFalse(fileExists(OUT_XAST_DIR.resolve("mod_no_claw.xast")));
    }

    public void testForceInput() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("translation/without_src_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("translation/without_src_files/reference");
        final Path OUT_XAST_DIR = TMP_DIR.resolve("xast");
        List<String> xastNames = Arrays.asList("mod11.xast", "mod12.xast", "mod13.xast", "mod_no_claw.xast", "p1.xast");
        String[] args = new String[] { "--stop-trans", "--force", "--disable-mp", "-TXO", OUT_XAST_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : xastNames)
        {
            Path resFilePath = OUT_XAST_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
    }

    public void testMultiprocessing() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("translation/without_src_files/input/1.f90");
        final Path REF_MOD_DIR = RES_DIR.resolve("translation/without_src_files/reference");
        final Path OUT_XAST_DIR = TMP_DIR.resolve("xast");
        List<String> xastNames = Arrays.asList("mod11.xast", "mod12.xast", "mod13.xast", "mod_no_claw.xast", "p1.xast");
        String[] args = new String[] { "--stop-trans", "--force", "-TXO", OUT_XAST_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        for (String modName : xastNames)
        {
            Path resFilePath = OUT_XAST_DIR.resolve(modName);
            Path refFilePath = REF_MOD_DIR.resolve(modName);
            String resStr = removeType(removeTime(collectIntoString(resFilePath)));
            String refStr = removeType(removeTime(collectIntoString(refFilePath)));
            assertEquals(refStr, resStr);
        }
    }
}
