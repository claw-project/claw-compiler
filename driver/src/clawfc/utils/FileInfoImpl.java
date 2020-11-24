/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

public class FileInfoImpl implements clawfc.FileInfo
{
    final Path path;
    final FileTime ts;

    public FileInfoImpl(Path path) throws IOException
    {
        this.path = path;
        ts = Files.getLastModifiedTime(path);

    }

    public Path getPath()
    {
        return path;
    }

    public FileTime getLastModifiedTS()
    {
        return ts;
    }
}