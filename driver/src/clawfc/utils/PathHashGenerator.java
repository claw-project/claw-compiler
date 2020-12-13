/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.nio.file.Path;

public interface PathHashGenerator
{
    public String generate(Path path);
}