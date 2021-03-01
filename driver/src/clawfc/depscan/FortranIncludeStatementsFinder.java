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
import java.util.ArrayList;
import java.util.List;
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
 * Finds Fortran include statements
 */
public class FortranIncludeStatementsFinder
{
    static class Listener extends FortranIncludesResolverBaseListener
    {
        Exception _error;
        List<FortranStatementBasicPosition> includes;
        FortranIncludeStatementRecognizer recognizer;

        public Exception error()
        {
            return _error;
        }

        public Listener() throws IOException
        {
            _error = null;
            includes = new ArrayList<FortranStatementBasicPosition>();
            recognizer = new FortranIncludeStatementRecognizer();
        }

        @Override
        public void exitInclude_line(FortranIncludesResolverParser.Include_lineContext ctx)
        {
            String txt = ctx.getText();
            final int startChrIdx = Utils.getStartChrIdx(ctx);
            final int endChrIdx = startChrIdx + txt.length();
            try
            {
                String filePathStr = recognizer.run(txt);
                includes.add(new FortranStatementBasicPosition(filePathStr, startChrIdx, endChrIdx));
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException("FortranIncludeStatementRecognizer failed");
            }
        }
    }

    FortranIncludesResolverLexer lexer;
    FortranIncludesResolverParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    /**
     * @throws IOException
     */
    public FortranIncludeStatementsFinder() throws IOException
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
     * @throws Exception
     */
    public List<FortranStatementBasicPosition> run(InputStream input) throws Exception
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
        return listener.includes;
    }
}
