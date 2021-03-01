/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import java.nio.file.Path;
import java.nio.file.attribute.FileTime;

public interface FileInfo
{
    public Path getPath();

    public FileTime getLastModifiedTS();
}
