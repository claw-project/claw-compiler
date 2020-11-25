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

import clawfc.depscan.parser.FortranCommentsFilterBaseListener;
import clawfc.depscan.parser.FortranCommentsFilterLexer;
import clawfc.depscan.parser.FortranCommentsFilterParser;

public class FortranCommentsFilter
{
    static class Listener extends FortranCommentsFilterBaseListener
    {
        final OutputStream outStrm;

        public IOException error()
        {
            return _error;
        }

        IOException _error;
        final FilteredContent content;

        public Listener(OutputStream outStrm, FilteredContent content)
        {
            this.outStrm = outStrm;
            this.content = content;
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
            String comment = ctx.getText();
            int startChrIdx = ctx.getStart().getStartIndex();
            if (startChrIdx < 0)
            {
                throw new RuntimeException("org.antlr.v4.runtime.Token.getStartIndex not implemented");
            }
            content.addSequence(startChrIdx, comment);
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

    public FilteredContent run(InputStream input, OutputStream output) throws FortranSyntaxException, IOException
    {
        FilteredContent content = new FilteredContent();
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
        Listener listener = new Listener(output, content);
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
        return content;
    }
}
