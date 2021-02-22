/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.copy;
import static clawfc.ut.PreprocessorOutputScannerTest.readPathsFromFile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import clawfc.AddIgnoreDirectiveFilter;
import clawfc.Configuration;
import clawfc.Preprocessor;
import clawfc.Preprocessor.PreprocessorInfo;
import clawfc.TrailingBackslashCommentsFilter;
import clawfc.Utils;
import clawfc.depscan.FortranIncludesResolver;
import clawfc.depscan.PreprocessorOutputScanner;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class PreprocessorTest extends TestCase
{
    protected final Path RES_DIR = clawfc.ut.Resources.DIR;
    Configuration DRIVER_CFG;

    @Override
    protected void setUp() throws Exception
    {
        DRIVER_CFG = new Configuration();
    }

    Set<Path> readRelativePathsFromFile(Path dir, Path refIncFilesLstFilePath) throws IOException
    {
        Set<Path> paths = readPathsFromFile(refIncFilesLstFilePath);
        Set<Path> res = new LinkedHashSet<Path>();
        for (Path path : paths)
        {
            res.add(dir.resolve(path));
        }
        return res;
    }

    String collectWithoutEmptyLines(InputStream inStrm) throws IOException
    {
        AsciiArrayIOStream buf = new AsciiArrayIOStream();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inStrm));
                PrintWriter writer = new PrintWriter(buf))
        {
            while (reader.ready())
            {
                String line = reader.readLine();
                if (!line.trim().isEmpty())
                {
                    writer.println(line);
                }
            }
        }
        return collectIntoString(buf.getAsInputStreamUnsafe());
    }

    void verifyPP(String testName, List<String> relPPIncludeDirs, List<String> predefinedMacros, String accDirLanguage,
            Path inputFilePath) throws Exception
    {
        Path IN_DIR = RES_DIR.resolve("preprocessor/" + testName + "/input");
        Path REF_DIR = RES_DIR.resolve("preprocessor/" + testName + "/reference");
        if (inputFilePath == null)
        {
            inputFilePath = IN_DIR.resolve("in.f90");
        }
        List<Path> ppIncludeDirs = relPPIncludeDirs.stream().map(p -> IN_DIR.resolve(p)).collect(Collectors.toList());
        Path refFilePath = REF_DIR.resolve("in.pp.f90");
        Path refIncFilesLstFilePath = REF_DIR.resolve("in.inc.lst");
        Path workingDir = Files.createTempDirectory(null);
        workingDir.toFile().deleteOnExit();
        try
        {
            PreprocessorInfo info = new PreprocessorInfo(DRIVER_CFG.defaultFortranCompilerCmd(),
                    DRIVER_CFG.defaultFortranCompilerVendor());
            List<String> cmdArgsTemplate = Collections
                    .unmodifiableList(Preprocessor.prepareArgs(info, accDirLanguage, predefinedMacros));
            PreprocessorOutputScanner outputScanner = new PreprocessorOutputScanner();
            Set<Path> resIncFilePaths = new LinkedHashSet<Path>();
            FortranIncludesResolver includesResolver = new FortranIncludesResolver();
            AddIgnoreDirectiveFilter addIgnoreFilter = new AddIgnoreDirectiveFilter();
            TrailingBackslashCommentsFilter trailingBSFilter = new TrailingBackslashCommentsFilter();
            AsciiArrayIOStream res = Preprocessor.run(inputFilePath, resIncFilePaths, workingDir, info, cmdArgsTemplate,
                    includesResolver, ppIncludeDirs, outputScanner, addIgnoreFilter, trailingBSFilter);
            String refStr = collectWithoutEmptyLines(Files.newInputStream(refFilePath));
            String resStr = collectWithoutEmptyLines(res.getAsInputStreamUnsafe());
            Set<Path> refIncPaths = readRelativePathsFromFile(IN_DIR, refIncFilesLstFilePath);
            assertEquals(refStr, resStr);
            assertEquals(refIncPaths, resIncFilePaths);
        } finally
        {
            if (workingDir != null)
            {
                Utils.removeDir(workingDir);
            }
        }
    }

    void verifyPP(String testName, List<String> relPPIncludeDirs, List<String> predefinedMacros, String accDirLanguage)
            throws Exception
    {
        verifyPP(testName, relPPIncludeDirs, predefinedMacros, accDirLanguage, null);
    }

    void verifyPP(String testName, List<String> relPPIncludeDirs, List<String> predefinedMacros) throws Exception
    {
        verifyPP(testName, relPPIncludeDirs, predefinedMacros, null);
    }

    void verifyPP(String testName, List<String> relPPIncludeDirs) throws Exception
    {
        verifyPP(testName, relPPIncludeDirs, Collections.emptyList(), null);
    }

    void verifyPP(String testName, List<String> relPPIncludeDirs, Path inPath) throws Exception
    {
        verifyPP(testName, relPPIncludeDirs, Collections.emptyList(), null, inPath);
    }

    public void testSameDir() throws Exception
    {
        verifyPP("same_dir", Collections.emptyList());
    }

    public void testAboveDir() throws Exception
    {
        verifyPP("above_dir", Arrays.asList("inc"));
    }

    public void testSpaceInDir() throws Exception
    {
        verifyPP("space_in_dir", Arrays.asList("i n c"));
    }

    public void testSpaceInFileName() throws Exception
    {
        verifyPP("space_in_filename", Collections.emptyList());
    }

    public void testIfdef() throws Exception
    {
        verifyPP("ifdef", Collections.emptyList(), Arrays.asList("macro"));
    }

    public void testAcceleratorDirectiveLanguage() throws Exception
    {
        verifyPP("acc_dir_language", Collections.emptyList(), Collections.emptyList(), "openmp");
    }

    public void testFortranIncludes() throws Exception
    {
        verifyPP("fortran_includes", Arrays.asList("inc"));
    }

    public void testTrailingBackslashRemoval() throws Exception
    {
        verifyPP("trailing_backslash", Collections.emptyList());
    }

    public void testBigInput() throws Exception
    {
        Path workingDir = Files.createTempDirectory(null);
        workingDir.toFile().deleteOnExit();
        try
        {
            final int N_KB = 100;
            final String bigInput;
            {
                final String line = String.join("", Collections.nCopies(64, " ")) + "\n";
                final String kbStr = String.join("", Collections.nCopies(16, line)) + "\n";
                bigInput = "#ifdef bla\n" + String.join("", Collections.nCopies(N_KB, kbStr)) + "#endif\nend\n";
            }

            final Path inFilePath = workingDir.resolve("in.f90");
            try (OutputStream out = Files.newOutputStream(inFilePath))
            {
                copy(Utils.toInputStream(bigInput), out);
            }
            verifyPP("big_input", Collections.emptyList(), inFilePath);
            Files.delete(inFilePath);
        } finally
        {
            if (workingDir != null)
            {
                Utils.removeDir(workingDir);
            }
        }
    }

}
