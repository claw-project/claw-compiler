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

import clawfc.parsers.CLAWVerbatimLineRecognizerBaseListener;
import clawfc.parsers.CLAWVerbatimLineRecognizerLexer;
import clawfc.parsers.CLAWVerbatimLineRecognizerParser;
import clawfc.utils.ParserErrorListener;

public class VerbatimDirectiveRecognizer
{
    static class Listener extends CLAWVerbatimLineRecognizerBaseListener
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
        public void exitContents(CLAWVerbatimLineRecognizerParser.ContentsContext ctx)
        {
            try
            {
                String txt = ctx.getText();// |contents|
                contents = txt;
            } catch (Exception e)
            {
                onError(e);
            }
        }
    }

    CLAWVerbatimLineRecognizerLexer lexer;
    CLAWVerbatimLineRecognizerParser parser;
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

    public VerbatimDirectiveRecognizer() throws Exception
    {
        lexer = new CLAWVerbatimLineRecognizerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new CLAWVerbatimLineRecognizerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
