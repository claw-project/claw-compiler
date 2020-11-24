/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranIncludeFilterBaseListener;
import clawfc.depscan.parser.FortranIncludeFilterLexer;
import clawfc.depscan.parser.FortranIncludeFilterParser;

/**
 * Replaces Fortran include with cpp preprocessor include
 */
public class FortranIncludeFilter
{
    static class Listener extends FortranIncludeFilterBaseListener
    {
        OutputStream outStrm;
        IOException _error;
        boolean _includeFound;

        public boolean includeFound()
        {
            return _includeFound;
        }

        public IOException error()
        {
            return _error;
        }

        public Listener(OutputStream outStrm)
        {
            _error = null;
            this.outStrm = outStrm;
        }

        @Override
        public void exitInclude_line(FortranIncludeFilterParser.Include_lineContext ctx)
        {
            _includeFound = true;
            output("#" + ctx.getText().stripLeading());
        }

        @Override
        public void exitOther_line(FortranIncludeFilterParser.Other_lineContext ctx)
        {
            output(ctx.getText());
        }

        void output(String s)
        {
            byte[] bytes = s.getBytes(StandardCharsets.US_ASCII);
            try
            {
                outStrm.write(bytes);
            } catch (IOException e)
            {
                _error = e;
                throw new CancellationException("Write error");
            }
        }
    }

    FortranIncludeFilterLexer lexer;
    FortranIncludeFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    /**
     * @throws IOException
     */
    public FortranIncludeFilter() throws IOException
    {
        lexer = new FortranIncludeFilterLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranIncludeFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    /**
     * @return true if at least one include statement found
     */
    public boolean run(InputStream input, OutputStream output) throws IOException, FortranSyntaxException
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
        Listener listener = new Listener(output);
        try
        {
            walker.walk(listener, tree);
        } catch (CancellationException e)
        {
        }
        if (listener.error() != null)
        {
            throw listener.error();
        }
        return listener.includeFound();
    }
}
