/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Collection;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

public class Utils
{
    public static CommonTokenStream toTokenStream(Lexer lexer, String str) throws IOException
    {
        CharStream chrStrm = toCharStream(str);
        lexer.reset();
        lexer.setInputStream(chrStrm);
        CommonTokenStream tokStrm = new CommonTokenStream(lexer);
        return tokStrm;
    }

    public static int getStartChrIdx(org.antlr.v4.runtime.ParserRuleContext ctx)
    {
        int startChrIdx = ctx.getStart().getStartIndex();
        if (startChrIdx < 0)
        {
            throw new RuntimeException("org.antlr.v4.runtime.Token.getStartIndex not implemented");
        }
        return startChrIdx;
    }

    public static Path findFile(String includeStr, Collection<Path> searchPath)
    {
        for (Path dir : searchPath)
        {
            Path testPath = dir.resolve(includeStr);
            if (clawfc.Utils.fileExists(testPath))
            {
                return testPath;
            }
        }
        return null;
    }

    public static byte[] readNBytes(InputStream strm, int nBytes) throws IOException
    {
        final byte[] bytes = new byte[nBytes];
        if (strm.read(bytes, 0, nBytes) != nBytes)
        {
            throw new IOException("read failed");
        }
        return bytes;
    }
}
