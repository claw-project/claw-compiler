/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.depscan.FortranCommentsFilter;
import clawfc.depscan.FortranLineBreaksFilter;
import clawfc.depscan.FortranSourceRecognitionException;
import clawfc.depscan.parser.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
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

class LinesBreaksFilterListener
    extends FortranLineBreaksFilterBaseListener
{
    public ArrayList<String> lineBreaks = new ArrayList<String>();
    public String txt;
    
    @Override
    public void exitUnclosed_line_break(FortranLineBreaksFilterParser.Unclosed_line_breakContext ctx) 
    {
        //System.out.println(ctx.getText());
        lineBreaks.add(ctx.getText());
    }     
    @Override
    public void exitFortran_text(FortranLineBreaksFilterParser.Fortran_textContext ctx) 
    {
        //System.out.println(ctx.getText());
        txt = ctx.getText();
    }  
} 


public class FortranLineBreaksFilterTest 
    extends TestCase 
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
        assertTrue(String.format("Failed to accept string \"%s\"", str), parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0); 
        if(breaks != null)
        {
            assertEquals(breaks.length, listener.lineBreaks.size());
            for(int i = 0; i < breaks.length; ++i)
            {
                assertEquals(breaks[i], listener.lineBreaks.get(i));
            }            
        }
        if(filteredStr != null)
        { 
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
    

	private void verifyFilter(String in, String expectedOut)
	{
		 InputStream inStrm = new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII));
	     ByteArrayOutputStream outStrm = new ByteArrayOutputStream();
	     try
	     {
	    	 filter = new FortranLineBreaksFilter();
	         filter.run(inStrm, outStrm);            
	     }
	     catch(FortranSourceRecognitionException e)
	     {
	         assertTrue("FortranSourceRecognitionException thrown", false);
	     }
	     catch(IOException e)
	     {
	         assertTrue("IOException thrown", false);
	     }
	     String res = outStrm.toString();
	     System.out.println(res);
	     assertEquals(expectedOut, res);
	}
    
    public void testFullLineBreakRemoval() throws Exception
    {
        verifyLineBreaksFilter("\n",  "\n" );
        verifyLineBreaksFilter("bla\n",  "bla\n" );
        verifyLineBreaksFilter("abra&\n"+
                               "&kadabra\n",  "abrakadabra\n" );
        verifyLineBreaksFilter("abra& \r\t\n"+
                               "&kadabra\n",  "abrakadabra\n" );
        verifyLineBreaksFilter("abra& \r\t\n"+
                               " \r\t&kadabra\n",  "abrakadabra\n" );
        verifyLineBreaksFilter("abra& \r\t\n"+
                               "\r\t\n"+
                               " \r\t&kadabra\n",  "abrakadabra\n" );
        verifyLineBreaksFilter("abra& \r\t\n"+
                               " \r\t\n"+
                               " \r\t\n"+
                               " \r\t&kadabra\n",  "abrakadabra\n" );
        verifyLineBreaksFilter("abra& \r\t\n"+
                " \r\t\n"+
                " \r\t\n"+
                " \r\t&kadabra polu&\n"+
                " \r\t&ndra\n",  "abrakadabra polundra\n" );
        verifyLineBreaksFilter("module&\n"+
                               "x\n",  
                               "module&\n"+
                               "x\n");
    }
    
    public void testUnclosedLineBreakExtraction() throws Exception
    {
        verifyLineBreaksFilter("\n",  new String[] {});
        verifyLineBreaksFilter("bla\n",  new String[] {} );
        verifyLineBreaksFilter("module&\n"+
                               "x\n",  new String[] {"&\n"} );
        verifyLineBreaksFilter("module&\r\t\n"+
                               "x\n",  new String[] {"&\r\t\n"} );
        verifyLineBreaksFilter("module&\r\t\n"+
                               "\r\tx\n",  new String[] {"&\r\t\n\r\t"}  );
        verifyLineBreaksFilter("module&\r\t\n"+
                               "\r\t\n"+
                               "\r\tx\n",  new String[] {"&\r\t\n\r\t\n\r\t"} );
        verifyLineBreaksFilter("module&\r\t\n"+
                "\r\t\n"+
                "\r\t\n"+
                "\r\tx\n",  new String[] {"&\r\t\n\r\t\n\r\t\n\r\t"} );
        verifyLineBreaksFilter("module&\n"+
                               " end& \n"+
                               "x\n",  new String[] {"&\n ", "& \n"} );
    }
    
    public void testFilter() throws Exception
    {
    	String expectedRes = "module x\n" + 
	                         "end module x\n";
    	verifyFilter("module x\n" + 
    			     "end module x\n", expectedRes);
    	verifyFilter("mod&\n" + 
    			     "&ule x\n" +
			         "end module x\n", expectedRes);
    	verifyFilter("mod&\n   " + 
			     "\r\r\n" +
			     "   &ule x\n" +
		         "end module x\n", expectedRes);
    	verifyFilter("mod&\n" + 
			         "&ule x\n" +
		             "end mod&\n" + 
			         "&ule x\n", expectedRes);
    	verifyFilter("mod&\n" + 
		         "&ule x\n" +
	             "end&  \n"+
		         "mod&\n" + 
		         "&ule x\n", expectedRes);
    	verifyFilter("mod&\n" + 
		         "&ule x\n" +
	             "end&  \n"+
		         "mod&\n" + 
		         "&ule x\n", expectedRes);
    	verifyFilter("mod&\n" + 
		         "&ule x\n" +
	             "end&  \n"+
		         " mod&\n" + 
		         "&ule x\n", expectedRes);
    }
}
