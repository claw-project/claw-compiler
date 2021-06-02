/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.utils;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class SimplePathHashGenerator implements PathHashGenerator
{
    final MessageDigest digest;

    /**
     * @throws NoSuchAlgorithmException
     */
    public SimplePathHashGenerator() throws NoSuchAlgorithmException
    {
        digest = MessageDigest.getInstance("SHA-1");
    }

    /**
     * Not thread safe
     */
    public String generate(Path path)
    {
        return generate(path, false);
    }

    /**
     * Not thread safe
     */
    protected String generate(Path path, boolean normalized)
    {
        if (!normalized)
        {
            path = path.normalize();
        }
        byte[] pathStr = path.toString().getBytes(StandardCharsets.US_ASCII);
        digest.update(pathStr);
        byte[] digestBytes = digest.digest();
        String digestStr = bytesToHex(digestBytes);
        return digestStr;
    }

    private static String bytesToHex(byte[] hash)
    {
        StringBuilder hexString = new StringBuilder(2 * hash.length);
        for (int i = 0; i < hash.length; i++)
        {
            String hex = Integer.toHexString(0xff & hash[i]);
            if (hex.length() == 1)
            {
                hexString.append('0');
            }
            hexString.append(hex);
        }
        return hexString.toString();
    }

}
