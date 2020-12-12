/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.collectIntoString;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.List;

public class SubprocessFailed extends Exception
{
    final String stdin;
    final String stderr;
    final List<String> args;

    public SubprocessFailed(List<String> args, InputStream stdin, InputStream stderr) throws IOException
    {
        super();
        this.stdin = stdin != null ? collectIntoString(stdin) : null;
        this.stderr = collectIntoString(stderr);
        this.args = Collections.unmodifiableList(args);
    }

    public String toErrorString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("Call arguments:\n");
        for (String arg : args)
        {
            sb.append("\t").append(arg).append("\n");
        }
        sb.append("stdin:\n\t\"").append(stdin).append("\"\n");
        sb.append("stderr:\n\t\"").append(stderr).append("\"\n");
        return sb.toString();
    }
}