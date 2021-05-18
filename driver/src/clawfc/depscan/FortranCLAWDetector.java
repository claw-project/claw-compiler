/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranCLAWScannerBaseListener;
import clawfc.depscan.parser.FortranCLAWScannerLexer;
import clawfc.depscan.parser.FortranCLAWScannerParser;

/**
 * <code>FortranCLAWDetector</code> wraps ANTLR-generated parser for detecting
 * presence of CLAW directives in Fortran source files.
 *
 */
public class FortranCLAWDetector
{
    static class Listener extends FortranCLAWScannerBaseListener
    {
        public boolean clawFound;

        void stop()
        {
            throw new CancellationException();
        }

        public Listener()
        {
            clawFound = false;
        }

        @Override
        public void exitClaw_directive_line(FortranCLAWScannerParser.Claw_directive_lineContext ctx)
        {
            clawFound = true;
            stop();
        }

        @Override
        public void exitClaw_guard_line(FortranCLAWScannerParser.Claw_guard_lineContext ctx)
        {
            clawFound = true;
            stop();
        }
    }

    FortranCLAWScannerLexer lexer;
    FortranCLAWScannerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranCLAWDetector() throws IOException
    {
        lexer = new FortranCLAWScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranCLAWScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    public boolean run(InputStream input) throws IOException, FortranSyntaxException
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
        {
            tree = parser.root();
        } catch (CancellationException e)
        {
        }
        if (lexerErrorListener.error() != null)
        {
            throw lexerErrorListener.error();
        }
        if (parserErrorListener.error() != null)
        {
            throw parserErrorListener.error();
        }
        ParseTreeWalker walker = new ParseTreeWalker();
        Listener listener = new Listener();
        try
        {
            walker.walk(listener, tree);
        } catch (CancellationException e)
        {
        }
        return listener.clawFound;
    }
}
