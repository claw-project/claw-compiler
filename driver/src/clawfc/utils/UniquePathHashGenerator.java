/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import java.nio.file.Path;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class UniquePathHashGenerator extends SimplePathHashGenerator
{
    final Map<Path, String> pathToHash;
    final Set<String> allocatedHashes;

    public UniquePathHashGenerator() throws NoSuchAlgorithmException
    {
        super();
        pathToHash = new HashMap<Path, String>();
        allocatedHashes = new HashSet<String>();
    }

    public synchronized String generate(Path path)
    {
        path = path.normalize();
        String hash = pathToHash.get(path);
        if (hash != null)
        {
            return hash;
        }
        hash = super.generate(path, true);
        if (allocatedHashes.contains(hash))
        {// Very unlikely event
            Path hPath = path;
            do
            {
                hPath = hPath.resolve("_");
                hash = super.generate(path, true);
            } while (allocatedHashes.contains(hash));
        }
        allocatedHashes.add(hash);
        pathToHash.put(path, hash);
        return hash;
    }

}