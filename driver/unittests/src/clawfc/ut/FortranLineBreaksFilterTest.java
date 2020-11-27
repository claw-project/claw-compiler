/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.FilteredContentSequence;
import clawfc.depscan.FortranLineBreaksFilter;
import clawfc.depscan.FortranSyntaxException;
import clawfc.depscan.parser.FortranLineBreaksFilterBaseListener;
import clawfc.depscan.parser.FortranLineBreaksFilterLexer;
import clawfc.depscan.parser.FortranLineBreaksFilterParser;
import junit.framework.TestCase;

class LinesBreaksFilterListener extends FortranLineBreaksFilterBaseListener
{
    public ArrayList<String> lineBreaks = new ArrayList<String>();
    ByteArrayOutputStream buf = new ByteArrayOutputStream();
    public String txt;

    @Override
    public void exitUnclosed_line_break(FortranLineBreaksFilterParser.Unclosed_line_breakContext ctx)
    {
        // System.out.println(ctx.getText());
        lineBreaks.add(ctx.getText());
    }

    /*
     * @Override public void exitOther(FortranLineBreaksFilterParser.OtherContext
     * ctx) { String s = ctx.getText(); byte[] bytes =
     * s.getBytes(StandardCharsets.US_ASCII); buf.write(bytes)
     * //System.out.println(ctx.getText()); }
     */

    @Override
    public void exitFortran_text(FortranLineBreaksFilterParser.Fortran_textContext ctx)
    {
        // System.out.println(ctx.getText());
        txt = ctx.getText();
    }
}

public class FortranLineBreaksFilterTest extends TestCase
{
    private FortranLineBreaksFilterParser parser;
    private FortranLineBreaksFilterLexer lexer;
    private FortranLineBreaksFilter filter;

    @Override
    protected void setUp() throws Exception
    {
        lexer = new FortranLineBreaksFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranLineBreaksFilterParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        filter = new FortranLineBreaksFilter();
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

    private void verifyLineBreaksFilter(String str, String filteredStr, String[] breaks) throws Exception
    {
        LinesBreaksFilterListener listener = new LinesBreaksFilterListener();
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.setBuildParseTree(true);
        ParseTree tree = parser.root();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);
        assertTrue(String.format("Failed to accept string \"%s\"", str),
                parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
        if (breaks != null)
        {
            assertEquals(breaks.length, listener.lineBreaks.size());
            for (int i = 0; i < breaks.length; ++i)
            {
                assertEquals(breaks[i], listener.lineBreaks.get(i));
            }
        }
        if (filteredStr != null)
        {
            System.out.println(listener.txt);
            assertEquals(filteredStr, listener.txt);
        }
    }

    private void verifyLineBreaksFilter(String str, String filteredStr) throws Exception
    {
        verifyLineBreaksFilter(str, filteredStr, null);
    }

    private void verifyLineBreaksFilter(String str, String[] breaks) throws Exception
    {
        verifyLineBreaksFilter(str, null, breaks);
    }

    private void verifyFilter(String in, String expectedOut, boolean preserveNumLines,
            List<Integer> expectedEOLSubstLines)
    {
        InputStream inStrm = new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII));
        ByteArrayOutputStream outStrm = new ByteArrayOutputStream();
        List<Integer> EOLSubstLines = null;
        FilteredContentSequence cRes = null;
        try
        {
            filter = new FortranLineBreaksFilter();
            if (expectedEOLSubstLines == null)
            {
                cRes = filter.run(inStrm, outStrm, preserveNumLines, null);
            } else
            {
                EOLSubstLines = new ArrayList<Integer>();
                cRes = filter.run(inStrm, outStrm, preserveNumLines, EOLSubstLines);
            }
        } catch (FortranSyntaxException e)
        {
            assertTrue("FortranSyntaxException thrown", false);
        } catch (IOException e)
        {
            assertTrue("IOException thrown", false);
        }
        String res = outStrm.toString();
        /* System.out.println("----------\n" + res + "\\n----------\n"); */
        assertEquals(expectedOut, res);
        assertEquals(expectedEOLSubstLines, EOLSubstLines);
        String testRev = cRes.partialReverse(res, '_');
        for (int i = 0, n = testRev.length(); i < n; ++i)
        {
            char fChr = testRev.charAt(i);
            if (fChr != '_')
            {
                char iChr = in.charAt(i);
                if (iChr != fChr)
                {
                    cRes.getOriginalChrIdx(20);
                    String testRev1 = cRes.get(1).partialReverse(res, '_');
                    System.out.println(in + "\n");
                    System.out.println(testRev + "\n");
                    System.out.println(testRev1);
                    assertEquals(iChr, fChr);
                }
            }
        }
    }

    private void verifyFilter(String in, String expectedOut, boolean preserveNumLines)
    {
        verifyFilter(in, expectedOut, preserveNumLines, null);
    }

    private void verifyFilter(String in, String expectedOut)
    {
        verifyFilter(in, expectedOut, false, null);
    }

    public void testUnclosedLineBreakExtraction() throws Exception
    {
        verifyLineBreaksFilter("\n", new String[] {});
        verifyLineBreaksFilter("bla\n", new String[] {});
        verifyLineBreaksFilter("module&\n" + "x\n", new String[] { "&\n" });
        verifyLineBreaksFilter("module&\r\t\n" + "x\n", new String[] { "&\r\t\n" });
        verifyLineBreaksFilter("module&\r\t\n" + "\r\tx\n", new String[] { "&\r\t\n\r\t" });
        verifyLineBreaksFilter("module&\r\t\n" + "\r\t\n" + "\r\tx\n", new String[] { "&\r\t\n\r\t\n\r\t" });
        verifyLineBreaksFilter("module&\r\t\n" + "\r\t\n" + "\r\t\n" + "\r\tx\n",
                new String[] { "&\r\t\n\r\t\n\r\t\n\r\t" });
        verifyLineBreaksFilter("module&\n" + " end& \n" + "x\n", new String[] { "&\n ", "& \n" });
    }

    public void testFilter() throws Exception
    {
        String expectedRes = "module x\n" + "end module x\n";
        verifyFilter("module x\n" + "end module x\n", expectedRes);
        verifyFilter("mod&\n" + "&ule x\n" + "end module x\n", expectedRes);
        verifyFilter("mod&\n   " + "\r\r\n" + "   &ule x\n" + "end module x\n", expectedRes);
        verifyFilter("mod&\n" + "&ule x\n" + "end mod&\n" + "&ule x\n", expectedRes);
        verifyFilter("mod&\n" + "&ule x\n" + "end&  \n" + "mod&\n" + "&ule x\n", expectedRes);
        verifyFilter("mod&\n" + "&ule x\n" + "end&  \n" + "mod&\n" + "&ule x\n", expectedRes);
        verifyFilter("mod&\n" + "&ule x\n" + "end&  \n" + " mod&\n" + "&ule x\n", expectedRes);
    }

    public void testFilterWithLinesNumPreservation() throws Exception
    {

        {
            String in = "module x\n" + "end module x\n";
            String expectedRes = "module x\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, new ArrayList<Integer>());
        }
        {
            String in = "mod&\n" + "&ule x\n" + "end module x\n";
            String expectedRes = "\n" + "module x\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, Arrays.asList(0));
        }
        {
            String in = "mod&\n   " + "\r\r\n" + "   &ule x\n" + "end module x\n";
            String expectedRes = "\n" + "\n" + "module x\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, Arrays.asList(0, 1));
        }

        {
            String in = "mod&\n" + "&ule x\n" + "end mod&\n" + "&ule x\n";
            String expectedRes = "\n" + "module x\n" + "\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, Arrays.asList(0, 2));
        }
        {
            String in = "mod&\n" + "&ule x\n" + "end&  \n" + "mod&\n" + "&ule x\n";
            String expectedRes = "\n" + "module x\n" + "\n" + "\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, Arrays.asList(0, 2, 3));
        }
        {
            String in = "m&   \n" + "&od&\n" + "    &ule&    \n" + "x\n" + "end module x\n";
            String expectedRes = "\n" + "\n" + "\n" + "module x\n" + "end module x\n";
            verifyFilter(in, expectedRes, true, Arrays.asList(0, 1, 2));
        }
    }
}
