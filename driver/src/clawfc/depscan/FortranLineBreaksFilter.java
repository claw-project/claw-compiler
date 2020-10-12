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
        public ArrayList<IOException> errors;
        String lineBreakSep;
        
        public Listener(OutputStream outStrm)
        {
        	initialise(outStrm, " ");
        }
        
        public Listener(OutputStream outStrm, String lineBreakSep)
        {
        	initialise(outStrm, lineBreakSep);
        }
        
        void initialise(OutputStream outStrm, String lineBreakSep)
        {
            this.outStrm = outStrm;
            comments = new ArrayList<String>();
            errors = new ArrayList<IOException>();
            this.lineBreakSep = lineBreakSep;
        }
        @Override
        public void exitOther(FortranLineBreaksFilterParser.OtherContext ctx)
        {
        	output(ctx.getText());
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
        	output(sep);
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
                errors.add(e);
            }
        }
    }

    FortranLineBreaksFilterLexer lexer;
    FortranLineBreaksFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;
    
    public FortranLineBreaksFilter() throws IOException
    {
        lexer = new FortranLineBreaksFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranLineBreaksFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
    
    public void run(InputStream input, OutputStream output) throws FortranSourceRecognitionException, IOException
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
        ParseTree tree = parser.root();
        ParseTreeWalker walker = new ParseTreeWalker();
        Listener listener = new Listener(output);
        walker.walk(listener, tree);
        if(!lexerErrorListener.errors.isEmpty())
        { throw lexerErrorListener.errors.get(0); }
        if(!parserErrorListener.errors.isEmpty())
        { throw parserErrorListener.errors.get(0); }
        if(!listener.errors.isEmpty())
        { throw listener.errors.get(0); }
    }
    
    static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.US_ASCII));  
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.US_ASCII);
        return chrStrm;
    }
}
