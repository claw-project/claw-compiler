/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.getExtension;
import static clawfc.Utils.removeExtension;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import clawfc.depscan.FortranFileBuildInfo;
import clawfc.utils.AsciiArrayIOStream;

public class PreprocessedFortranSourceData
{
    AsciiArrayIOStream ppSrc;
    final List<Path> incFilePaths;
    final Path ppSrcFilePath;
    final String srcFilename;

    public synchronized AsciiArrayIOStream getPPSource() throws IOException
    {
        if (ppSrc == null)
        {
            ppSrc = new AsciiArrayIOStream(ppSrcFilePath);
        }
        return ppSrc;
    }

    public List<Path> getIncludeFilePaths()
    {
        return incFilePaths;
    }

    private PreprocessedFortranSourceData(Path srcFilePath, AsciiArrayIOStream ppSrc, List<Path> incFilePaths,
            Path ppSrcFilePath)
    {
        this.ppSrc = ppSrc;
        this.incFilePaths = incFilePaths;
        this.ppSrcFilePath = ppSrcFilePath;
        this.srcFilename = srcFilePath.getFileName().toString();
    }

    /* Load existing preprocessed source from path in info */
    public static PreprocessedFortranSourceData load(Path srcFilePath, FortranFileBuildInfo info) throws IOException
    {
        Path ppSrcFilePath = info.getPPSrcFilePath();
        PreprocessedFortranSourceData data = new PreprocessedFortranSourceData(srcFilePath, null, info.getIncludes(),
                ppSrcFilePath);
        return data;
    }

    /* Preprocess input source file */
    public static PreprocessedFortranSourceData load(Path srcFilePath, Preprocessor preprocessor) throws Exception
    {
        Set<Path> incFilePaths = new HashSet<Path>();
        AsciiArrayIOStream ppSrc;
        try
        {
            ppSrc = preprocessor.run(srcFilePath, incFilePaths);
        } catch (Preprocessor.Failed e)
        {
            String errMsg = sprintf("Exception thrown while preprocessing input file \"%s\":\n%s", srcFilePath,
                    e.getMessage());
            throw new Exception(errMsg, e);
        }
        PreprocessedFortranSourceData data = new PreprocessedFortranSourceData(srcFilePath, ppSrc,
                new ArrayList<Path>(incFilePaths), null);
        return data;
    }

    /* Load already preprocessed input source file */
    public static PreprocessedFortranSourceData load(Path ppSrcFilePath) throws IOException
    {
        List<Path> incFilePaths = Collections.emptyList();
        PreprocessedFortranSourceData data = new PreprocessedFortranSourceData(ppSrcFilePath, null,
                new ArrayList<Path>(incFilePaths), ppSrcFilePath);
        return data;
    }

    /**
     * @return Output file path
     * @throws Exception
     */
    public Path save(Path outDirPath, String srcDirHash) throws Exception
    {
        if (ppSrcFilePath != null)
        {
            Path loadDirPath = ppSrcFilePath.getParent().normalize();
            outDirPath = outDirPath.normalize();
            if (loadDirPath.equals(outDirPath))
            {// It is the same file that was loaded, no need to save it
                return ppSrcFilePath;
            }
        }
        Path outFilePath = getOutputFilePath(srcFilename, outDirPath, srcDirHash);
        saveToFile(getPPSource().getAsInputStreamUnsafe(), outFilePath);
        return outFilePath;
    }

    public static Path getOutputFilePath(String srcFileName, Path outDir, String srcDirHash)
    {
        String basename = removeExtension(srcFileName);
        String extension = getExtension(srcFileName);
        String outFilename = sprintf("%s_%s.pp.%s", srcDirHash, basename, extension);
        Path outFilePath = outDir.resolve(outFilename);
        return outFilePath;
    }

}