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

public class FortranCommentsFilter
{
    static class Listener
        extends FortranCommentsFilterBaseListener
    {
        OutputStream outStrm;
        public ArrayList<String> comments;
        public ArrayList<IOException> errors;
        
        public Listener(OutputStream outStrm)
        {
            this.outStrm = outStrm;
            comments = new ArrayList<String>();
            errors = new ArrayList<IOException>();
        }

        @Override
        public void exitOther(FortranCommentsFilterParser.OtherContext ctx)
        {
            byte[] s = ctx.getText().getBytes(StandardCharsets.US_ASCII);
            try
            {
                outStrm.write(s);
            }
            catch(IOException e)
            {
                errors.add(e);
            }
        }

        @Override
        public void exitString(FortranCommentsFilterParser.StringContext ctx)
        {
            byte[] s = ctx.getText().getBytes(StandardCharsets.US_ASCII);
            try
            {
                outStrm.write(s);
            }
            catch(IOException e)
            {
                errors.add(e);
            }
        }

        @Override
        public void exitComment(FortranCommentsFilterParser.CommentContext ctx)
        {
            try
            {
                outStrm.write(new byte[] {'\n'});
            }
            catch(IOException e)
            {
                errors.add(e);
            }
            String comment = ctx.getText();
            comments.add(comment);            
        }
    }

    FortranCommentsFilterLexer lexer;
    FortranCommentsFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;
    
    public FortranCommentsFilter() throws IOException
    {
        lexer = new FortranCommentsFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranCommentsFilterParser(new CommonTokenStream(lexer));
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
