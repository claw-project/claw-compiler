/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.collectIntoString;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

public class SubprocessFailed extends Exception
{
    public SubprocessFailed(List<String> args, InputStream stdin, InputStream stderr) throws IOException
    {
        super(toErrorString(args, stdin, stderr));
    }

    static String toErrorString(List<String> args, InputStream stdin, InputStream stderr) throws IOException
    {
        String stdinStr = stdin != null ? collectIntoString(stdin) : "";
        String stderrStr = stderr != null ? collectIntoString(stderr) : "";
        StringBuilder sb = new StringBuilder();
        sb.append("Subprocess failed\n");
        sb.append("Call arguments:\n");
        for (String arg : args)
        {
            sb.append("\t").append(arg).append("\n");
        }
        sb.append("stdin:\n\t\"").append(stdinStr).append("\"\n");
        sb.append("stderr:\n\t\"").append(stderrStr).append("\"\n");
        return sb.toString();
    }
}