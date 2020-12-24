/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

public interface FileInfo
{
    public Path getPath();

    public FileTime getLastModifiedTS();
}
