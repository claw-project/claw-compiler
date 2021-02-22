/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc;

import static clawfc.Utils.toAscii;
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

import clawfc.depscan.ParserErrorListener;
import clawfc.depscan.parser.FortranCommentsFilterBaseListener;
import clawfc.depscan.parser.FortranCommentsFilterLexer;
import clawfc.depscan.parser.FortranCommentsFilterParser;
import clawfc.utils.AsciiArrayIOStream;

public class TrailingBackslashCommentsFilter
{
    static class Listener extends FortranCommentsFilterBaseListener
    {
        final OutputStream outStrm;

        public Exception error()
        {
            return _error;
        }

        Exception _error;
        final CommentTrailingBackslashRecognizer bsRecognizer;
        final AsciiArrayIOStream buf;

        public Listener(OutputStream outStrm) throws Exception
        {
            this.outStrm = outStrm;
            _error = null;
            bsRecognizer = new CommentTrailingBackslashRecognizer();
            buf = new AsciiArrayIOStream();
        }

        @Override
        public void exitOther_token(FortranCommentsFilterParser.Other_tokenContext ctx)
        {
            output(ctx.getText());
        }

        @Override
        public void exitString(FortranCommentsFilterParser.StringContext ctx)
        {
            output(ctx.getText());
        }

        void onError(Exception e)
        {
            _error = e;
            throw new CancellationException();
        }

        @Override
        public void exitComment(FortranCommentsFilterParser.CommentContext ctx)
        {
            String comment = ctx.getText();
            try
            {
                buf.write(toAscii(comment));
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitEol(FortranCommentsFilterParser.EolContext ctx)
        {
            String eol = ctx.getText();
            try
            {
                if (buf.size() > 0)
                {
                    buf.write(toAscii(eol));
                    String filteredComment = bsRecognizer.parse(buf.getAsInputStreamUnsafe());
                    output(filteredComment);
                } else
                {
                    output(eol);
                }
                buf.reset();

            } catch (Exception e)
            {
                onError(e);
            }
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

    public TrailingBackslashCommentsFilter() throws IOException
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

    public void run(InputStream input, OutputStream output) throws Exception
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
    }
}
