/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

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

import clawfc.depscan.parser.FortranIncludesResolverBaseListener;
import clawfc.depscan.parser.FortranIncludesResolverLexer;
import clawfc.depscan.parser.FortranIncludesResolverParser;

/**
 * Detects Fortran include statements
 */
public class FortranIncludeChecker
{
    static class Listener extends FortranIncludesResolverBaseListener
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

        public Listener()
        {
            _error = null;
        }

        @Override
        public void exitInclude_line(FortranIncludesResolverParser.Include_lineContext ctx)
        {
            _includeFound = true;
        }
    }

    FortranIncludesResolverLexer lexer;
    FortranIncludesResolverParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    /**
     * @throws IOException
     */
    public FortranIncludeChecker() throws IOException
    {
        lexer = new FortranIncludesResolverLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranIncludesResolverParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    /**
     * @return true if at least one include statement found
     */
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
        if (listener.error() != null)
        {
            throw listener.error();
        }
        return listener.includeFound();
    }
}
