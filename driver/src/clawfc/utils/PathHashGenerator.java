/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class PathHashGenerator
{
    final MessageDigest digest;

    /**
     * @throws NoSuchAlgorithmException
     */
    public PathHashGenerator() throws NoSuchAlgorithmException
    {
        digest = MessageDigest.getInstance("SHA-1");
    }

    /**
     * Not thread safe
     */
    public String generate(Path path)
    {
        byte[] pathStr = path.normalize().toString().getBytes(StandardCharsets.US_ASCII);
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