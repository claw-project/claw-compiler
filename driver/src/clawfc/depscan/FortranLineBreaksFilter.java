/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class FortranLineBreaksFilter
{
    static class Listener
        extends FortranLineBreaksFilterBaseListener
    {
        OutputStream outStrm;
        public ArrayList<String> comments;
        public IOException error() { return _error; }
        IOException _error;
        String lineBreakSep;
        boolean preserveNumLines;
        ArrayList<String> buf;
        
        public Listener(OutputStream outStrm)
        {
        	initialise(outStrm, " ", false);
        }
        
        public Listener(OutputStream outStrm, String lineBreakSep, boolean preserveNumLines)
        {
        	initialise(outStrm, lineBreakSep, preserveNumLines);
        }
        
        void initialise(OutputStream outStrm, String lineBreakSep, boolean preserveNumLines)
        {
        	_error = null;
            this.outStrm = outStrm;
            comments = new ArrayList<String>();
            this.lineBreakSep = lineBreakSep;
            this.preserveNumLines = preserveNumLines;
            if(this.preserveNumLines)
            { buf = new ArrayList<String>(); }
        }
        @Override
        public void exitOther(FortranLineBreaksFilterParser.OtherContext ctx)
        {
        	String s = ctx.getText();
        	if(!preserveNumLines)
        	{ output(s); }
        	else
        	{ buf.add(s); }        	
        }
        
        void outputBuf()
        {
        	for(String s: buf)
        	{ output(s); }
        	buf.clear();
        }
        
        @Override
        public void exitEol(FortranLineBreaksFilterParser.EolContext ctx)
        {
        	if(preserveNumLines)
        	{ outputBuf(); }
        	output("\n");
        }
        
        @Override
        public void exitFortran_text(FortranLineBreaksFilterParser.Fortran_textContext ctx)
        {
        	if(preserveNumLines)
        	{ outputBuf(); }
        }
        
        void outputEOLs(String s)
        {
        	for(char c: s.toCharArray())
        	{ 
        		if(c == '\n')
        		{ output("\n"); }
        	}
        }
        
        @Override
        public void exitUnclosed_line_break(FortranLineBreaksFilterParser.Unclosed_line_breakContext ctx)
        {
        	String text = ctx.getText();
        	int EOLIdx = text.lastIndexOf('\n');
        	String sep = text.substring(EOLIdx + 1);
        	if(sep.isEmpty())
        	{
        		sep = this.lineBreakSep;
        	}
        	if(!preserveNumLines)
        	{ output(sep); }
        	else
        	{ 
        		buf.add(sep);
        		outputEOLs(text);
        	}
        }

        @Override
        public void exitClosed_line_break(FortranLineBreaksFilterParser.Closed_line_breakContext ctx)
        {
        	String text = ctx.getText();
        	if(preserveNumLines)
        	{ outputEOLs(text); }        	
        }
        
        void output(String s)
        {
        	byte[] bytes = s.getBytes(StandardCharsets.US_ASCII);
            try
            {
                outStrm.write(bytes);
            }
            catch(IOException e)
            {
            	_error = e;
            	throw new CancellationException("Write error");
            }
        }
    }

    FortranLineBreaksFilterLexer lexer;
    FortranLineBreaksFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;
    
    public FortranLineBreaksFilter() throws IOException
    {
        lexer = new FortranLineBreaksFilterLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranLineBreaksFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
    
    public void run(InputStream input, OutputStream output, boolean preserveNumLines) throws FortranSyntaxException, IOException
    {
        lexer.reset();
        parser.reset();
        lexerErrorListener.reset();
        parserErrorListener.reset();
        CharStream chrStrm = CharStreams.fromStream(input, StandardCharsets.US_ASCII);
        lexer.setInputStream(chrStrm);
        CommonTokenStream tokStrm = new CommonTokenStream(lexer);
        parser.setInputStream(tokStrm);
        parser.setBuildParseTree(true);
        ParseTree tree = null;
        try
        { tree = parser.root(); }
        catch(CancellationException e)
        {}
        if(lexerErrorListener.error() != null)
        { throw lexerErrorListener.error(); }
        if(parserErrorListener.error() != null)
        { throw parserErrorListener.error(); }
        ParseTreeWalker walker = new ParseTreeWalker();
        Listener listener = new Listener(output, " ", preserveNumLines);
        try
        { walker.walk(listener, tree); }
        catch(CancellationException e)
        {}        
        if(listener.error() != null)
        { throw listener.error(); }
    }
    
    public void run(InputStream input, OutputStream output) throws FortranSyntaxException, IOException
    {
    	run(input, output, false);
    }
}
