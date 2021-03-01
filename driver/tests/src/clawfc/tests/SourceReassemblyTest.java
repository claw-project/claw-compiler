/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.tests;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SourceReassemblyTest extends clawfc.tests.utils.DriverTestCase
{
    public void testOutputFile() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("source_reassembly/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("source_reassembly/reference/1.f90");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        final Path OUT_FILEPATH = OUT_SRC_DIR.resolve("1.f90");
        String[] args = new String[] { "--debug", "--add-paren", "--disable-mp", "-o", OUT_FILEPATH.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        assertEqualsTxtFiles(REF_FILEPATH, OUT_FILEPATH);
    }

    public void testOutputDir() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("source_reassembly/input/1.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("source_reassembly/reference/1.f90");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        final Path OUT_FILEPATH = OUT_SRC_DIR.resolve("1.f90");
        String[] args = new String[] { "--debug", "--add-paren", "--disable-mp", "-O", OUT_SRC_DIR.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        assertEqualsTxtFiles(REF_FILEPATH, OUT_FILEPATH);
    }

    public void testEmptyFile() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("source_reassembly/input/empty.f95");
        final Path REF_FILEPATH = RES_DIR.resolve("source_reassembly/reference/empty.f95");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        final Path OUT_FILEPATH = OUT_SRC_DIR.resolve("empty.f95");
        String[] args = new String[] { "--debug", "--add-paren", "--disable-mp", "-o", OUT_FILEPATH.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        assertEqualsTxtFiles(REF_FILEPATH, OUT_FILEPATH);
    }

    public void testIgnore() throws Exception
    {
        final Path INPUT_FILEPATH = RES_DIR.resolve("source_reassembly/input/ignore.f90");
        final Path REF_FILEPATH = RES_DIR.resolve("source_reassembly/reference/ignore.f90");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        final Path OUT_FILEPATH = OUT_SRC_DIR.resolve("ignore.f90");
        String[] args = new String[] { "--debug", "--add-paren", "--disable-mp", "-o", OUT_FILEPATH.toString(),
                INPUT_FILEPATH.toString() };
        run(args);
        assertEqualsTxtFiles(REF_FILEPATH, OUT_FILEPATH);
    }

    public void testMultiprocessing() throws Exception
    {
        List<String> filenames = Arrays.asList("1.f90", "2.f90", "empty.f95");
        final Path INPUT_DIR = RES_DIR.resolve("source_reassembly/input");
        final Path REF_DIR = RES_DIR.resolve("source_reassembly/reference");
        final Path OUT_SRC_DIR = TMP_DIR.resolve("out-src");
        List<String> argsLst = new ArrayList<String>(
                Arrays.asList("--debug", "--add-paren", "-O", OUT_SRC_DIR.toString()));
        for (String filename : filenames)
        {
            Path inputFilepath = INPUT_DIR.resolve(filename);
            argsLst.add(inputFilepath.toString());
        }
        String[] args = argsLst.toArray(new String[0]);
        run(args);
        for (String filename : filenames)
        {
            Path refFilepath = REF_DIR.resolve(filename);
            Path outFilepath = OUT_SRC_DIR.resolve(filename);
            assertEqualsTxtFiles(refFilepath, outFilepath);
        }
    }

    /*
     * public void testForceInput() throws Exception { final Path INPUT_FILEPATH =
     * RES_DIR.resolve("decompilation/input/1.f90"); final Path REF_MOD_DIR =
     * RES_DIR.resolve("decompilation/reference"); final Path OUT_SRC_DIR =
     * TMP_DIR.resolve("out-src"); List<String> outSrcFilenames =
     * Arrays.asList("mod11.f90", "mod12.f90", "mod13.f90", "p1.f90",
     * "mod_no_claw.f90"); String[] args = new String[] { "--force", "--add-paren",
     * "--stop-dec", "--disable-mp", "-TSO", OUT_SRC_DIR.toString(),
     * INPUT_FILEPATH.toString() }; run(args); for (String filename :
     * outSrcFilenames) { Path resFilePath = OUT_SRC_DIR.resolve(filename); Path
     * refFilePath = REF_MOD_DIR.resolve(filename); String resStr =
     * removeType(removeTime(collectIntoString(resFilePath))); String refStr =
     * removeType(removeTime(collectIntoString(refFilePath))); assertEquals(refStr,
     * resStr); } }
     * 
     * public void testMultiprocessing() throws Exception { final Path
     * INPUT_FILEPATH = RES_DIR.resolve("decompilation/input/1.f90"); final Path
     * REF_MOD_DIR = RES_DIR.resolve("decompilation/reference"); final Path
     * OUT_SRC_DIR = TMP_DIR.resolve("out-src"); List<String> outSrcFilenames =
     * Arrays.asList("mod11.f90", "mod12.f90", "mod13.f90", "p1.f90",
     * "mod_no_claw.f90"); String[] args = new String[] { "--force", "--add-paren",
     * "--stop-dec", "-TSO", OUT_SRC_DIR.toString(), INPUT_FILEPATH.toString() };
     * run(args); for (String filename : outSrcFilenames) { Path resFilePath =
     * OUT_SRC_DIR.resolve(filename); Path refFilePath =
     * REF_MOD_DIR.resolve(filename); String resStr =
     * removeType(removeTime(collectIntoString(resFilePath))); String refStr =
     * removeType(removeTime(collectIntoString(refFilePath))); assertEquals(refStr,
     * resStr); } }
     */
}
