/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import clawfc.depscan.PreprocessorLineMarkerRecognizer;
import clawfc.depscan.PreprocessorOutputScanner;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class PreprocessorOutputScannerTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;
    final Path IN_DIR = RES_DIR.resolve("preproc_scan/input");
    final Path REF_DIR = RES_DIR.resolve("preproc_scan/reference");

    void verifyLineMarkerString(PreprocessorLineMarkerRecognizer r, final String input, final String refFilepathStr)
            throws Exception
    {
        String res = r.run(input + "\n");
        assertEquals(refFilepathStr, res);
    }

    public void testPreprocessorLineMarkerRecognizer() throws Exception
    {
        PreprocessorLineMarkerRecognizer r = new PreprocessorLineMarkerRecognizer();
        verifyLineMarkerString(r, "# 1 \"filename\"", "filename");
        verifyLineMarkerString(r, "# 1 \"filename\" 1", "filename");
        verifyLineMarkerString(r, "# 1 \"/data/tmp/dep-test/i nc/m.inc\" 1", "/data/tmp/dep-test/i nc/m.inc");
        verifyLineMarkerString(r, "# 1 \"/data/tmp/dep-test/i\\\"nc/m.inc\" 1", "/data/tmp/dep-test/i\\\"nc/m.inc");
    }

    public static Set<Path> readPathsFromFile(Path incFilesLstFilePath) throws IOException
    {
        Set<Path> refIncPaths = new HashSet<Path>();
        {
            List<String> lines = Files.readAllLines(incFilesLstFilePath);
            for (String line : lines)
            {
                line = line.strip();
                if (!line.isEmpty())
                {
                    refIncPaths.add(Paths.get(line));
                }
            }
            refIncPaths = Collections.unmodifiableSet(refIncPaths);
        }
        return refIncPaths;
    }

    void verifyPreprocessorOutputScanner(PreprocessorOutputScanner scanner, String inputName) throws Exception
    {
        Path inputFilePath = IN_DIR.resolve(inputName + ".txt");
        Path refNoPPFilePath = REF_DIR.resolve(inputName + ".f90");
        Path refIncFilesLstFilePath = REF_DIR.resolve(inputName + ".inc");
        Set<Path> refIncPaths = readPathsFromFile(refIncFilesLstFilePath);
        AsciiArrayIOStream refNoPp = new AsciiArrayIOStream(refNoPPFilePath);
        AsciiArrayIOStream input = new AsciiArrayIOStream(inputFilePath);
        AsciiArrayIOStream resNoPp = new AsciiArrayIOStream();
        Set<Path> resIncPaths = scanner.run(input.getAsInputStreamUnsafe(), resNoPp);
        String refNoPpStr = collectIntoString(refNoPp.getAsInputStreamUnsafe());
        String resNoPpStr = collectIntoString(resNoPp.getAsInputStreamUnsafe());
        assertEquals(refNoPpStr, resNoPpStr);
        assertEquals(refIncPaths, resIncPaths);
    }

    public void testPreprocessorOutputScanner() throws Exception
    {
        PreprocessorOutputScanner scanner = new PreprocessorOutputScanner();
        verifyPreprocessorOutputScanner(scanner, "gfortran");
        verifyPreprocessorOutputScanner(scanner, "pgfortran");
        verifyPreprocessorOutputScanner(scanner, "intel");
        verifyPreprocessorOutputScanner(scanner, "cray");
    }
}
