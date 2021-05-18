/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
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

import clawfc.parsers.CLAWIgnoreLineRecognizerBaseListener;
import clawfc.parsers.CLAWIgnoreLineRecognizerLexer;
import clawfc.parsers.CLAWIgnoreLineRecognizerParser;
import clawfc.utils.ParserErrorListener;

/**
 * <code>IgnoreDirectiveRecognizer</code> wraps ANTLR-generated parser for
 * processing lines of Fortran code decorated with the CLAW ignore directive.
 *
 */
public class IgnoreDirectiveRecognizer
{
    static class Listener extends CLAWIgnoreLineRecognizerBaseListener
    {
        String contents;
        Exception error;

        public Listener()
        {
            error = null;
        }

        public Exception getError()
        {
            return error;
        }

        public String getContents()
        {
            return contents;
        }

        void onError(Exception e)
        {
            error = e;
            throw new CancellationException();
        }

        @Override
        public void exitContents(CLAWIgnoreLineRecognizerParser.ContentsContext ctx)
        {
            try
            {
                String txt = ctx.getText();// |contents|
                contents = txt.substring(1, txt.length() - 1);
            } catch (Exception e)
            {
                onError(e);
            }
        }
    }

    CLAWIgnoreLineRecognizerLexer lexer;
    CLAWIgnoreLineRecognizerParser parser;
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
        }
        if (listener.getError() != null)
        {
            throw listener.getError();
        }
        return listener.getContents();
    }

    public IgnoreDirectiveRecognizer() throws Exception
    {
        lexer = new CLAWIgnoreLineRecognizerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new CLAWIgnoreLineRecognizerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
