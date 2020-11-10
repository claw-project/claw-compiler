/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import clawfc.Utils.ByteArrayIOStream;
import clawfc.depscan.parser.FortranCLAWScannerBaseListener;
import clawfc.depscan.parser.FortranCLAWScannerLexer;
import clawfc.depscan.parser.FortranCLAWScannerParser;

public class FortranCLAWScanner
{
    static class Listener
        extends FortranCLAWScannerBaseListener
    {
        public final List<Integer> clawDirectives;
        public final List<Integer> clawGuards;
        public IOException error() { return _error; }
        IOException _error;
        
        public Listener()
        {
            _error = null;
            clawDirectives = new ArrayList<Integer>();
            clawGuards = new ArrayList<Integer>();
        }
        
        @Override
        public void exitClaw_directive_line(FortranCLAWScannerParser.Claw_directive_lineContext ctx)
        {
            clawDirectives.add(ctx.getStart().getLine());
        }
        
        @Override
        public void exitClaw_guard_line(FortranCLAWScannerParser.Claw_guard_lineContext ctx)
        {
            clawGuards.add(ctx.getStart().getLine());
        }
    }
    
    FortranCLAWScannerLexer lexer;
    FortranCLAWScannerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;
    
    public FortranCLAWScanner() throws IOException
    {
        lexer = new FortranCLAWScannerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranCLAWScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
    
    public FortranFileCLAWLinesInfo run(InputStream input) throws IOException, FortranSyntaxException
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
        Listener listener = new Listener();
        try
        { walker.walk(listener, tree); }
        catch(CancellationException e)
        {}        
        if(listener.error() != null)
        { throw listener.error(); }
        FortranFileCLAWLinesInfo res = new FortranFileCLAWLinesInfo(listener.clawDirectives,
                                                                    listener.clawGuards);
        return res;
    }
}
