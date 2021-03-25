/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.Utils;
import clawfc.depscan.FilteredContentSequence;
import clawfc.depscan.FortranCommentsFilter;
import clawfc.depscan.FortranSyntaxException;
import clawfc.depscan.parser.FortranCommentsFilterBaseListener;
import clawfc.depscan.parser.FortranCommentsFilterLexer;
import clawfc.depscan.parser.FortranCommentsFilterParser;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

class ExtractCommentsListener extends FortranCommentsFilterBaseListener
{
    ArrayList<String> comments = new ArrayList<String>();

    @Override
    public void exitComment(FortranCommentsFilterParser.CommentContext ctx)
    {
        // System.out.println(ctx.getText());
        comments.add(ctx.getText());
    }
}

public class FortranCommentsFilterTest extends TestCase
{
    private FortranCommentsFilterParser parser;
    private FortranCommentsFilterLexer lexer;
    FortranCommentsFilter filter;

    @Override
    protected void setUp() throws Exception
    {
        lexer = new FortranCommentsFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranCommentsFilterParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        filter = new FortranCommentsFilter();
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
        assertTrue(String.format("Failed to accept string \"%s\"", str),
                parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
        assertEquals(comments.length, listener.comments.size());
        for (int i = 0; i < comments.length; ++i)
        {
            assertEquals(comments[i], listener.comments.get(i));
        }
    }

    private void verifyFilter(String in, String expectedOut) throws IOException
    {
        ByteArrayIOStream inStrm = new ByteArrayIOStream();
        Utils.copy(new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII)), inStrm);
        ByteArrayIOStream outStrm = new ByteArrayIOStream();
        FortranCommentsFilter filter;
        FilteredContentSequence contentSeq = null;
        try
        {
            filter = new FortranCommentsFilter();
            contentSeq = filter.run(inStrm.getAsInputStreamUnsafe(), outStrm);
        } catch (FortranSyntaxException e)
        {
            assertTrue("FortranSyntaxException thrown", false);
        } catch (IOException e)
        {
            assertTrue("IOException thrown", false);
        }
        String res = outStrm.toString();
        assertEquals(expectedOut, res);
        // Verify filtered content
        ByteArrayIOStream revStrm = new ByteArrayIOStream();
        /*
         * content.reverse(outStrm.getAsInputStreamUnsafe(), revStrm); String revStr =
         * Utils.collectIntoString(revStrm.getAsInputStreamUnsafe()); assertEquals(in,
         * revStr);
         */
        for (int i = 0; i < res.length(); ++i)
        {
            int idx = contentSeq.getOriginalChrIdx(i);
            assertEquals(res.charAt(i), in.charAt(idx));
        }
    }

    public void testCommentsExtraction() throws Exception
    {
        verifyCommentsExtraction("!bla\n", new String[] { "!bla" });
        verifyCommentsExtraction("!bla1\n" + "!bla2\n", new String[] { "!bla1", "!bla2" });
        verifyCommentsExtraction("!bla1\n" + "'!bla2\n" + "'", new String[] { "!bla1" });
        verifyCommentsExtraction("!bla1\n" + "'\"bla2\n" + "\"", new String[] { "!bla1" });
        verifyCommentsExtraction("!bla1\n" + "'\"&\n" + "!bla2\n" + "\n" + "\"", new String[] { "!bla1" });
    }

    public void testFilter() throws Exception
    {
        verifyFilter("no comment", "no comment");
        verifyFilter("!bla\n", "\n");
        verifyFilter("code!comment\n", "code\n");
        verifyFilter("!bla1\n" + "!bla2\n", "\n\n");
        verifyFilter("!bla1\n" + "'!bla2\n" + "'", "\n" + "'!bla2\n" + "'");
        verifyFilter("!bla1\n" + "\"!bla2\n" + "\"", "\n" + "\"!bla2\n" + "\"");
    }
}
