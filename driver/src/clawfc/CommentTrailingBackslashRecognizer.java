/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc;

import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.parsers.CommentTrailingBackslashRecognizerBaseListener;
import clawfc.parsers.CommentTrailingBackslashRecognizerLexer;
import clawfc.parsers.CommentTrailingBackslashRecognizerParser;
import clawfc.utils.ParserErrorListener;

public class CommentTrailingBackslashRecognizer
{
    static class Listener extends CommentTrailingBackslashRecognizerBaseListener
    {
        StringBuilder contents;
        Exception error;

        public Listener()
        {
            error = null;
            contents = new StringBuilder();
        }

        public Exception getError()
        {
            return error;
        }

        void onError(Exception e)
        {
            error = e;
            throw new CancellationException();
        }

        public String getContents()
        {
            return contents.toString();
        }

        @Override
        public void exitTrailing_bs_seq(CommentTrailingBackslashRecognizerParser.Trailing_bs_seqContext ctx)
        {
            String txt = ctx.getText();
            if (txt.endsWith("\r\n"))
            {
                contents.append("\r\n");
            } else
            {
                contents.append("\n");
            }
        }

        @Override
        public void exitOther(CommentTrailingBackslashRecognizerParser.OtherContext ctx)
        {
            String txt = ctx.getText();
            contents.append(txt);
        }

        @Override
        public void exitBs(CommentTrailingBackslashRecognizerParser.BsContext ctx)
        {
            String txt = ctx.getText();
            contents.append(txt);
        }

        @Override
        public void exitEol(CommentTrailingBackslashRecognizerParser.EolContext ctx)
        {
            String txt = ctx.getText();
            contents.append(txt);
        }
    }

    CommentTrailingBackslashRecognizerLexer lexer;
    CommentTrailingBackslashRecognizerParser parser;
    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public String parse(InputStream input) throws Exception
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
            e.getMessage();
        }
        if (listener.getError() != null)
        {
            throw listener.getError();
        }
        return listener.getContents();
    }

    public CommentTrailingBackslashRecognizer() throws Exception
    {
        lexer = new CommentTrailingBackslashRecognizerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new CommentTrailingBackslashRecognizerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
