/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.depscan.parser.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.*;

import junit.framework.TestCase;

class ExtractCommentsListener
    extends FortranCommentsFilterBaseListener
{
    ArrayList<String> comments = new ArrayList<String>();
    
    @Override
    public void exitComment(FortranCommentsFilterParser.CommentContext ctx) 
    {
        //System.out.println(ctx.getText());
        comments.add(ctx.getText());
    }        
} 

public class FortranCommentsFilterTest 
    extends TestCase 
{
    private FortranCommentsFilterParser parser;
    private FortranCommentsFilterLexer lexer;
    
    @Override
    protected void setUp() throws Exception 
    {
        lexer = new FortranCommentsFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranCommentsFilterParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
    }
    
    private static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8));  
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.UTF_8);
        return chrStrm;
    }
    
    private CommonTokenStream toTokenStream(String str) throws IOException
    {
        CharStream chrStrm = toCharStream(str);
        lexer.reset();
        lexer.setInputStream(chrStrm);
        CommonTokenStream tokStrm = new CommonTokenStream(lexer);
        return tokStrm;
    }
    
    private void verifyCommentsExtraction(String str, String[] comments) throws Exception
    {
        ExtractCommentsListener listener = new ExtractCommentsListener();
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.setBuildParseTree(true);
        ParseTree tree = parser.root();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);
        assertTrue(String.format("Failed to accept string \"%s\"", str), parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0); 
        assertEquals(comments.length, listener.comments.size());
        for(int i = 0; i < comments.length; ++i)
        {
            assertEquals(comments[i], listener.comments.get(i));
        }
    }
    
    public void testCommentsExtraction() throws Exception
    {
        verifyCommentsExtraction("!bla\n", new String[] { "!bla\n" } ); 
        verifyCommentsExtraction("!bla1\n"+
                                 "!bla2\n", new String[] { "!bla1\n", "!bla2\n" } );
        verifyCommentsExtraction("!bla1\n"+
                                 "'!bla2\n"+
                                 "'", new String[] { "!bla1\n"} );
        verifyCommentsExtraction("!bla1\n"+
                                 "'\"bla2\n"+
                                 "\"", new String[] { "!bla1\n"} );
        verifyCommentsExtraction("!bla1\n"+
                                 "'\"&\n"+
                                 "!bla2\n"+
                                 "\n"+
                                 "\"", new String[] { "!bla1\n"} );
    }
}
