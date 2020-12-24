/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.collectIntoString;
import static clawfc.Utils.removeDir;
import static clawfc.Utils.saveToFile;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import clawfc.PreprocessedFortranSourceData;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.utils.AsciiArrayIOStream;
import junit.framework.TestCase;

public class PreprocessedFortranSourceDataTest extends TestCase
{
    public void testLoadFromExistingFile() throws Exception
    {

        Path tmpDir = Files.createTempDirectory(null);
        tmpDir.toFile().deleteOnExit();
        try
        {
            final String ref = "!Preprocessed src";
            AsciiArrayIOStream data = new AsciiArrayIOStream(ref);
            Path ppSrcFilePath = tmpDir.resolve("pp.src.f90");
            saveToFile(data.getAsInputStreamUnsafe(), ppSrcFilePath);
            PreprocessedFortranSourceData ppSrcData = PreprocessedFortranSourceData.load(ppSrcFilePath);
            assertEquals(Collections.emptyList(), ppSrcData.getIncludeFilePaths());
            final String res = collectIntoString(ppSrcData.getPPSource().getAsInputStreamUnsafe());
            assertEquals(ref, res);
        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }
    }

    public void testLoadWithBuildInfo() throws Exception
    {

        Path tmpDir = Files.createTempDirectory(null);
        tmpDir.toFile().deleteOnExit();
        try
        {
            final String ref = "!Preprocessed src";
            AsciiArrayIOStream data = new AsciiArrayIOStream(ref);
            Path srcFilePath = tmpDir.resolve("src.f90");
            Path ppSrcFilePath = tmpDir.resolve("pp.src.f90");
            saveToFile(data.getAsInputStreamUnsafe(), ppSrcFilePath);
            FortranFileBuildInfo info = new FortranFileBuildInfo(Collections.emptyList(), null);
            info.setPPSrcFilePath(ppSrcFilePath);
            Path incFilePath1 = tmpDir.resolve("1.inc");
            Path incFilePath2 = tmpDir.resolve("2.inc");
            List<Path> refIncPathsLst = Arrays.asList(incFilePath1, incFilePath2);
            info.setIncludes(refIncPathsLst);
            // ---------------------
            PreprocessedFortranSourceData ppSrcData = PreprocessedFortranSourceData.load(srcFilePath, info);
            assertEquals(refIncPathsLst, ppSrcData.getIncludeFilePaths());
            final String res = collectIntoString(ppSrcData.getPPSource().getAsInputStreamUnsafe());
            assertEquals(ref, res);
        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }
    }

    public void testSave() throws Exception
    {

        Path tmpDir = Files.createTempDirectory(null);
        tmpDir.toFile().deleteOnExit();
        try
        {
            final String ref = "!Preprocessed src";
            AsciiArrayIOStream data = new AsciiArrayIOStream(ref);
            Path ppSrcFilePath = tmpDir.resolve("src.f90");
            saveToFile(data.getAsInputStreamUnsafe(), ppSrcFilePath);
            PreprocessedFortranSourceData ppSrcData = PreprocessedFortranSourceData.load(ppSrcFilePath);
            // Save to different dir
            Path outDirPath = tmpDir.resolve("out");
            Files.createDirectory(outDirPath);
            final String hashStr = "hash";
            Path outPath1 = ppSrcData.save(outDirPath, hashStr);
            assertEquals(outDirPath, outPath1.getParent());
            assertEquals("hash_src.pp.f90", outPath1.getFileName().toString());
            final String res = collectIntoString(outPath1);
            assertEquals(ref, res);
            // Try to save to same dir
            final FileTime ts = Files.getLastModifiedTime(ppSrcFilePath);
            Path outPath2 = ppSrcData.save(tmpDir, hashStr);
            assertEquals(ppSrcFilePath, outPath2);
            final FileTime ts2 = Files.getLastModifiedTime(ppSrcFilePath);
            assertEquals(ts, ts2);// No save was performed, because the source was the same file
        } finally
        {
            if (tmpDir != null)
            {
                removeDir(tmpDir);
            }
        }
    }
}
