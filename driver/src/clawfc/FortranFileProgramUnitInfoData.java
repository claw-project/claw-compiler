/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.BuildInfo.BUILDINFO_FILE_EXTENSIONS;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.max;
import static clawfc.Utils.saveToFile;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

import clawfc.depscan.FortranFileProgramUnitInfo;
import clawfc.depscan.FortranFileProgramUnitInfoDeserializer;
import clawfc.depscan.FortranFileProgramUnitInfoSerializer;
import clawfc.utils.AsciiArrayIOStream;

public class FortranFileProgramUnitInfoData
{

    final FortranFileProgramUnitInfo info;
    final Path filePath;
    final FileTime ts;

    public FortranFileProgramUnitInfo getInfo()
    {
        return info;
    }

    public FileTime getTimestamp()
    {
        return ts;
    }

    public static class LoadFailed extends Exception
    {
        public LoadFailed(String errMsg)
        {
            super(errMsg);
        }

        public LoadFailed(String errMsg, Exception e)
        {
            super(errMsg, e);
        }
    }

    public static FortranFileProgramUnitInfoData load(Path filePath,
            FortranFileProgramUnitInfoDeserializer deserializer) throws LoadFailed, Exception
    {
        if (deserializer == null)
        {
            try
            {
                deserializer = new FortranFileProgramUnitInfoDeserializer(true);
            } catch (Exception e)
            {
                throw new LoadFailed(e.getMessage(), e);
            }
        }
        FortranFileProgramUnitInfo info;
        try (InputStream inStrm = Files.newInputStream(filePath))
        {
            info = deserializer.deserialize(inStrm);
        } catch (Exception e)
        {
            String errMsg = sprintf("Exception thrown while deserializing build information file \"%s\"",
                    filePath.toString());
            throw new LoadFailed(errMsg, e);
        }
        final FileTime infoTS = getFileTimestamp(filePath, "build information file");
        FileTime lastTS;
        {
            Path srcFilePath = info.getSrcFilePath();
            if (srcFilePath == null)
            {
                String errMsg = sprintf(
                        "Build information file \"%s\" cannot be verified. It does not contain source file path.",
                        filePath);
                throw new LoadFailed(errMsg);
            }
            lastTS = verifyReferencedFile(filePath, infoTS, "source", srcFilePath);
        }
        {
            Path ppSrcFilePath = info.getPPSrcFilePath();
            if (ppSrcFilePath == null)
            {
                String errMsg = sprintf(
                        "Build information file \"%s\" cannot be verified. It does not contain preprocessed source file path.",
                        filePath);
                throw new LoadFailed(errMsg);
            }
            FileTime ppSrcFileTS = verifyReferencedFile(filePath, infoTS, "preprocessed source", ppSrcFilePath);
            lastTS = max(lastTS, ppSrcFileTS);
        }
        for (Path incFilePath : info.getIncludes())
        {
            FileTime incFileTS = verifyReferencedFile(filePath, infoTS, "include", incFilePath);
            lastTS = max(lastTS, incFileTS);
        }
        return new FortranFileProgramUnitInfoData(filePath, info, lastTS);
    }

    static FileTime verifyReferencedFile(Path infoFilePath, FileTime infoTS, String refFileType, Path refFilePath)
            throws LoadFailed, Exception
    {
        FileTime refTS;
        if (!fileExists(refFilePath))
        {
            String errMsg = sprintf(
                    "Build information file \"%s\" is not valid. It refers to non-existing %s file \"%s\"",
                    infoFilePath, refFileType, refFilePath);
            throw new LoadFailed(errMsg);
        }
        {
            refTS = getFileTimestamp(refFilePath, refFileType);
            if (infoTS.compareTo(refTS) < 0)
            {
                String errMsg = sprintf(
                        "Build information file \"%s\" is not valid. It is older than referenced %s file \"%s\"",
                        infoFilePath, refFileType, refFilePath);
                throw new LoadFailed(errMsg);
            }
        }
        return refTS;
    }

    static FileTime getFileTimestamp(Path filePath, String fileType) throws Exception
    {
        if (filePath == null)
        {
            String errMsg = sprintf("%s file not set", fileType);
            throw new Exception(errMsg);
        }
        try
        {
            FileTime ts = Files.getLastModifiedTime(filePath);
            return ts;
        } catch (IOException e)
        {
            String errMsg = sprintf("Failed to stat %s file \"%s\"", fileType, filePath);
            throw new Exception(errMsg, e);
        }
    }

    static FileTime getLastTimestamp(FortranFileProgramUnitInfo info) throws Exception
    {
        FileTime lastTS;
        FileTime srcFileTS = getFileTimestamp(info.getSrcFilePath(), "source");
        lastTS = srcFileTS;
        if (info.getPPSrcFilePath() != null)
        {
            FileTime ppSrcFileTS = getFileTimestamp(info.getPPSrcFilePath(), "preprocessed source");
            lastTS = max(lastTS, ppSrcFileTS);
        }
        for (Path incFilePath : info.getIncludes())
        {
            FileTime incTS = getFileTimestamp(incFilePath, "include");
            lastTS = max(lastTS, incTS);
        }
        return lastTS;
    }

    /**
     * @return Output file path
     * @throws Exception
     */
    public Path save(Path outDirPath, String srcDirHash, FortranFileProgramUnitInfoSerializer serializer)
            throws Exception
    {
        if (filePath != null)
        {
            Path loadDirPath = filePath.getParent().normalize();
            outDirPath = outDirPath.normalize();
            if (loadDirPath.equals(outDirPath))
            {// It is the same file that was loaded, no need to save it
                return loadDirPath;
            }
        }
        if (serializer == null)
        {
            serializer = new FortranFileProgramUnitInfoSerializer();
        }
        String srcFilename = getInfo().getSrcFilePath().getFileName().toString();
        Path outFilePath = getOutputFilePath(srcFilename, outDirPath, srcDirHash);
        AsciiArrayIOStream serializedInfo = new AsciiArrayIOStream();
        serializer.serialize(info, serializedInfo);
        saveToFile(serializedInfo.getAsInputStreamUnsafe(), outFilePath);
        return outFilePath;
    }

    public static Path getOutputFilePath(String srcFilename, Path outDir, String srcDirHash)
    {
        String outFilename;
        if (srcDirHash != null)
        {
            outFilename = sprintf("%s_%s.%s", srcDirHash, srcFilename, BUILDINFO_FILE_EXTENSIONS[0]);
        } else
        {
            outFilename = sprintf("%s.%s", srcFilename, BUILDINFO_FILE_EXTENSIONS[0]);
        }
        Path outFilePath = outDir.resolve(outFilename);
        return outFilePath;
    }

    public FortranFileProgramUnitInfoData(FortranFileProgramUnitInfo info) throws Exception
    {
        this.filePath = null;
        this.info = info;
        this.ts = getLastTimestamp(info);
    }

    private FortranFileProgramUnitInfoData(Path filePath, FortranFileProgramUnitInfo info, FileTime ts)
    {
        this.filePath = filePath;
        this.info = info;
        this.ts = ts;
    }

}