/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

import clawfc.depscan.parser.*;

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
	
	public static InputStream toInputStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.US_ASCII));  
        return inStrm;
    }
	
	public static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = toInputStream(str);  
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.US_ASCII);
        return chrStrm;
    }
}
