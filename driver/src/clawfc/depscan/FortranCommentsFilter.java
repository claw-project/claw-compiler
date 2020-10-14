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

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class FortranCommentsFilter
{
    static class Listener
        extends FortranCommentsFilterBaseListener
    {
        OutputStream outStrm;
        public ArrayList<String> comments;
        public IOException error() { return _error; }
        IOException _error;
        
        public Listener(OutputStream outStrm)
        {
            this.outStrm = outStrm;
            comments = new ArrayList<String>();
            _error = null;
        }

        @Override
        public void exitOther(FortranCommentsFilterParser.OtherContext ctx)
        {
        	output(ctx.getText());
        }

        @Override
        public void exitString(FortranCommentsFilterParser.StringContext ctx)
        {
        	output(ctx.getText());
        }

        @Override
        public void exitComment(FortranCommentsFilterParser.CommentContext ctx)
        {
        	output("\n");
            String comment = ctx.getText();
            comments.add(comment);            
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

    FortranCommentsFilterLexer lexer;
    FortranCommentsFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;
    
    public FortranCommentsFilter() throws IOException
    {
        lexer = new FortranCommentsFilterLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranCommentsFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);        
    }
    
    public void run(InputStream input, OutputStream output) throws FortranSyntaxException, IOException
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
        Listener listener = new Listener(output);
        try
        { walker.walk(listener, tree); }
        catch(CancellationException e)
        {}
        if(listener.error() != null)
        { throw listener.error(); }
    }
}
