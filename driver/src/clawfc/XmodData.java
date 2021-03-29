/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.BuildInfo.XMOD_EXTENSION;
import static clawfc.Utils.saveToFile;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

import clawfc.utils.ByteArrayIOStream;
import clawfc.utils.FileInfo;

public class XmodData
{
    ByteArrayIOStream data;
    final String modName;
    final Path filePath;
    final FileTime ts;
    final boolean stdCompilerMod;

    /**
     * @return Module is part of compiler standard library
     */
    public boolean isStdCompilerMod()
    {
        return stdCompilerMod;
    }

    public Path getFilePath()
    {
        return filePath;
    }

    public FileTime getTimestamp()
    {
        return ts;
    }

    public synchronized ByteArrayIOStream getData() throws IOException
    {
        if (data == null)
        {
            data = new ByteArrayIOStream(filePath);
        }
        return data;
    }

    public XmodData(String modName, FileInfo xmodFileInfo, boolean stdCompilerMod)
    {
        this.modName = modName;
        data = null;
        filePath = xmodFileInfo.getPath();
        ts = xmodFileInfo.getLastModifiedTS();
        this.stdCompilerMod = stdCompilerMod;
    }

    public XmodData(String modName, FileInfo xmodFileInfo, ByteArrayIOStream data)
    {
        this.modName = modName;
        this.data = data;
        filePath = xmodFileInfo.getPath();
        ts = xmodFileInfo.getLastModifiedTS();
        stdCompilerMod = false;
    }

    /**
     * @return Output file path
     * @throws Exception
     */
    public Path save(Path outDirPath) throws Exception
    {
        Path outFilePath = getOutputFilePath(outDirPath, modName);
        if (filePath != null)
        {
            if (outFilePath.normalize().equals(filePath.normalize()))
            {// It is the same file that was loaded, no need to save it
                return outFilePath;
            }
        }
        saveToFile(getData().getAsInputStreamUnsafe(), outFilePath);
        return outFilePath;
    }

    public static Path getOutputFilePath(Path outDirPath, String modName)
    {
        Path outFilePath = outDirPath.resolve(modName + "." + XMOD_EXTENSION);
        return outFilePath;
    }

}
