/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.sprintf;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

public class FileInfoImpl implements FileInfo
{
    final Path path;
    final FileTime ts;

    public FileInfoImpl(Path path) throws Exception
    {
        this.path = path;
        try
        {
            ts = Files.getLastModifiedTime(path);
        } catch (IOException e)
        {
            throw new Exception(sprintf("Failed to stat \"%s\"", path), e);
        }
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