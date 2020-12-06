/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import clawfc.Utils;
import clawfc.depscan.FortranIncludeChecker;
import clawfc.depscan.FortranIncludeStatementRecognizer;
import clawfc.depscan.FortranIncludesResolver;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

public class FortranIncludeTest extends TestCase
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

    void assertTxtEqualsTxtFile(String resTxt, Path ref) throws Exception
    {
        String refTxt = readTxt(ref);
        assertEquals(refTxt, resTxt);
    }

    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    void verifyChecker(FortranIncludeChecker checker, Path inFilePath, boolean expRes) throws Exception
    {
        InputStream in = Files.newInputStream(inFilePath);
        assertEquals(expRes, checker.run(in));
    }

    public void testChecker() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("include_resolver/checker/input");
        final Path IN_YES_FILEPATH1 = IN_DIR.resolve("yes_1.f90");
        final Path IN_YES_FILEPATH2 = IN_DIR.resolve("yes_2.f90");
        final Path IN_YES_FILEPATH3 = IN_DIR.resolve("yes_3.f90");
        final Path IN_NO_FILEPATH1 = IN_DIR.resolve("no_1.f90");
        final Path IN_NO_FILEPATH2 = IN_DIR.resolve("no_2.f90");
        final Path IN_NO_FILEPATH3 = IN_DIR.resolve("no_3.f90");
        FortranIncludeChecker checker = new FortranIncludeChecker();
        verifyChecker(checker, IN_YES_FILEPATH1, true);
        verifyChecker(checker, IN_YES_FILEPATH2, true);
        verifyChecker(checker, IN_YES_FILEPATH3, true);
        verifyChecker(checker, IN_NO_FILEPATH1, false);
        verifyChecker(checker, IN_NO_FILEPATH2, false);
        verifyChecker(checker, IN_NO_FILEPATH3, false);
    }

    void verifyParsing(String incLine, String incString) throws Exception
    {
        FortranIncludeStatementRecognizer parser = new FortranIncludeStatementRecognizer();
        String res = parser.run(incLine);
        assertEquals(incString, res);
    }

    public void testParser() throws Exception
    {
        verifyParsing("include \"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"file.inc\"\n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"file.inc\"\r\t \n", "file.inc");
        verifyParsing(" \r\tinclude \r\t\"\"\"file.inc\"\r\t \n", "\"file.inc");
        verifyParsing(" \r\tinclude \r\t'file.inc'\r\t \n", "file.inc");
        verifyParsing(" \r\tinclude \r\t'''file.inc'\r\t \n", "'file.inc");
    }

    void verifyResolver(FortranIncludesResolver resolver, Path dir, String name) throws Exception
    {
        Path inputDir = dir.resolve("input");
        Path refDir = dir.resolve("reference");
        Path inputFile = inputDir.resolve(name + ".f90");
        Path refFile = refDir.resolve(name + ".f90");
        Path refIncLstFile = refDir.resolve(name + "_includes.txt");
        List<Path> searchPath = Arrays.asList(inputDir);
        ByteArrayIOStream output = new ByteArrayIOStream();
        AsciiArrayIOStream inputStrm = new AsciiArrayIOStream(inputFile);
        Set<Path> includeFiles = resolver.run(inputFile, inputStrm, output, searchPath);
        String resTxt = Utils.collectIntoString(output.getAsInputStreamUnsafe());
        assertTxtEqualsTxtFile(resTxt, refFile);
        List<String> refIncFilenames = Files.readAllLines(refIncLstFile, StandardCharsets.US_ASCII);
        refIncFilenames.removeIf(s -> s.strip().isEmpty());
        assertEquals(refIncFilenames.size(), includeFiles.size());
        for (String refIncFilename : refIncFilenames)
        {
            Path refIncFilepath = inputDir.resolve(refIncFilename);
            assertTrue(includeFiles.contains(refIncFilepath));
        }
    }

    public void testResolverNormal() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("include_resolver/normal");
        FortranIncludesResolver resolver = new FortranIncludesResolver();
        verifyResolver(resolver, IN_DIR, "empty");
        verifyResolver(resolver, IN_DIR, "1");
        verifyResolver(resolver, IN_DIR, "2");
        verifyResolver(resolver, IN_DIR, "nested");
        verifyResolver(resolver, IN_DIR, "line_breaks");
        verifyResolver(resolver, IN_DIR, "line_breaks_and_comment");
    }

    public void testResolverErrorMissingFile() throws Exception
    {
        FortranIncludesResolver resolver = new FortranIncludesResolver();
        final Path IN_DIR = RES_DIR.resolve("include_resolver/errors/input");
        Path inputFilePath = IN_DIR.resolve("missing.f90");
        AsciiArrayIOStream inputStrm = new AsciiArrayIOStream(inputFilePath);
        List<Path> searchPath = Arrays.asList(IN_DIR);
        ByteArrayIOStream output = new ByteArrayIOStream();
        boolean exceptionCaught = false;
        try
        {
            resolver.run(inputFilePath, inputStrm, output, searchPath);
        } catch (Exception e)
        {
            exceptionCaught = true;
            String errMsg = e.getMessage();
            assertTrue(errMsg.contains("Error while trying to resolve include at"));
            assertTrue(errMsg.contains("missing.f90:1"));
            assertTrue(errMsg.contains("Failed to find file \"missing.inc\" under given search path:"));
            assertTrue(errMsg.contains("driver/unittests/res/include_resolver/errors/input ;"));
        }
        assertTrue(exceptionCaught);
    }

    public void testResolverErrorInCludeStack() throws Exception
    {
        FortranIncludesResolver resolver = new FortranIncludesResolver();
        final Path IN_DIR = RES_DIR.resolve("include_resolver/errors/input");
        Path inputFilePath = IN_DIR.resolve("missing_with_stack.f90");
        AsciiArrayIOStream inputStrm = new AsciiArrayIOStream(inputFilePath);
        List<Path> searchPath = Arrays.asList(IN_DIR);
        ByteArrayIOStream output = new ByteArrayIOStream();
        boolean exceptionCaught = false;
        try
        {
            resolver.run(inputFilePath, inputStrm, output, searchPath);

        } catch (Exception e)
        {
            exceptionCaught = true;
            String errMsg = e.getMessage();
            assertTrue(errMsg.contains("Error while trying to resolve include at"));
            assertTrue(errMsg.contains("missing_with_stack.f90:1"));
            assertTrue(errMsg.contains("Failed to find file \"missing.inc\" under given search path:"));
            assertTrue(errMsg.contains("Include stack:"));
            assertTrue(errMsg.contains("m12.inc:1"));
            assertTrue(errMsg.contains("m23.inc:1"));
        }
        assertTrue(exceptionCaught);
    }

    public void testResolverErrorRecursive() throws Exception
    {
        FortranIncludesResolver resolver = new FortranIncludesResolver();
        final Path IN_DIR = RES_DIR.resolve("include_resolver/errors/input");
        Path inputFilePath = IN_DIR.resolve("recursive.f90");
        AsciiArrayIOStream inputStrm = new AsciiArrayIOStream(inputFilePath);
        List<Path> searchPath = Arrays.asList(IN_DIR);
        ByteArrayIOStream output = new ByteArrayIOStream();
        boolean exceptionCaught = false;
        try
        {
            resolver.run(inputFilePath, inputStrm, output, searchPath);
        } catch (Exception e)
        {
            exceptionCaught = true;
            String errMsg = e.getMessage();
            assertTrue(errMsg.contains("Error while trying to resolve include at"));
            assertTrue(errMsg.contains("recursive.inc:1"));
            assertTrue(errMsg.contains("recursive.inc\" is already included. Recursive includes are forbidden."));
        }
        assertTrue(exceptionCaught);
    }

}
