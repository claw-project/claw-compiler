/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import static clawfc.BuildInfo.BUILDINFO_FILE_EXTENSIONS;
import static clawfc.Utils.fileExists;
import static clawfc.Utils.max;
import static clawfc.Utils.removeExtension;
import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.FileTime;

import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranFileBuildInfoDeserializer;
import clawfc.depscan.FortranFileBuildInfoSerializer;

public class FortranFileBuildInfoData
{

    final FortranFileBuildInfo info;
    final Path filePath;
    final FileTime ts;

    public FortranFileBuildInfo getInfo()
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

    public static FortranFileBuildInfoData load(Path filePath, FortranFileBuildInfoDeserializer deserializer)
            throws LoadFailed, Exception
    {
        if (deserializer == null)
        {
            try
            {
                deserializer = new FortranFileBuildInfoDeserializer(true);
            } catch (Exception e)
            {
                throw new LoadFailed(e.getMessage(), e);
            }
        }
        FortranFileBuildInfo info;
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
        return new FortranFileBuildInfoData(filePath, info, lastTS);
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

    static FileTime getLastTimestamp(FortranFileBuildInfo info) throws Exception
    {
        FileTime lastTS;
        FileTime srcFileTS = getFileTimestamp(info.getSrcFilePath(), "source");
        lastTS = srcFileTS;
        FileTime ppSrcFileTS = getFileTimestamp(info.getPPSrcFilePath(), "preprocessed source");
        lastTS = max(lastTS, ppSrcFileTS);
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
    public Path save(Path dirPath, String hash, FortranFileBuildInfoSerializer serializer) throws Exception
    {
        if (filePath != null)
        {
            Path loadDirPath = filePath.getParent().normalize();
            dirPath = dirPath.normalize();
            if (loadDirPath.equals(dirPath))
            {// It is the same file that was loaded, no need to save it
                return loadDirPath;
            }
        }
        if (serializer == null)
        {
            serializer = new FortranFileBuildInfoSerializer();
        }
        String basename = removeExtension(getInfo().getSrcFilePath().getFileName().toString());
        String outFilename = sprintf("%s_%s.%s", hash, basename, BUILDINFO_FILE_EXTENSIONS[0]);
        Path outFilePath = dirPath.resolve(outFilename);
        Path outTmpFilePath = Files.createTempFile(dirPath, null, null);
        try (OutputStream out = Files.newOutputStream(outTmpFilePath))
        {
            serializer.serialize(info, out);
        } catch (Exception e)
        {
            Files.deleteIfExists(outTmpFilePath);
            throw e;
        }
        Files.move(outTmpFilePath, outFilePath, StandardCopyOption.ATOMIC_MOVE);
        return outFilePath;
    }

    public FortranFileBuildInfoData(FortranFileBuildInfo info) throws Exception
    {
        this.filePath = null;
        this.info = info;
        this.ts = getLastTimestamp(info);
    }

    private FortranFileBuildInfoData(Path filePath, FortranFileBuildInfo info, FileTime ts)
    {
        this.filePath = filePath;
        this.info = info;
        this.ts = ts;
    }

}