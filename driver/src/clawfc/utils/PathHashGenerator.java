/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import java.nio.file.Path;

public interface PathHashGenerator
{
    public String generate(Path path);
}
