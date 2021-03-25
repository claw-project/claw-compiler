/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.toAscii;

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

import clawfc.parsers.CLAWRemoveIgnoreScannerBaseListener;
import clawfc.parsers.CLAWRemoveIgnoreScannerLexer;
import clawfc.parsers.CLAWRemoveIgnoreScannerParser;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ParserErrorListener;

public class RemoveIgnoreDirectiveFilter
{
    static class Listener extends CLAWRemoveIgnoreScannerBaseListener
    {
        OutputStream output;
        final IgnoreDirectiveRecognizer directiveRecognizer;
        final AsciiArrayIOStream buf;

        public Exception getError()
        {
            return error;
        }

        Exception error;

        public Listener(OutputStream output, IgnoreDirectiveRecognizer directiveRecognizer)
        {
            this.output = output;
            this.directiveRecognizer = directiveRecognizer;
            error = null;
            buf = new AsciiArrayIOStream();
        }

        void onError(Exception e)
        {
            error = e;
            throw new CancellationException();
        }

        @Override
        public void exitOther_line(CLAWRemoveIgnoreScannerParser.Other_lineContext ctx)
        {
            try
            {
                final String line = ctx.getText();
                output.write(toAscii(line));
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitClaw_ignore_escaped_line(CLAWRemoveIgnoreScannerParser.Claw_ignore_escaped_lineContext ctx)
        {
            try
            {
                buf.reset();
                buf.write(toAscii(ctx.getText()));
                final String contents = directiveRecognizer.parse(buf.getAsInputStreamUnsafe()) + "\n";
                output.write(toAscii(contents));
            } catch (Exception e)
            {
                onError(e);
            }
        }
    }

    final CLAWRemoveIgnoreScannerLexer lexer;
    final CLAWRemoveIgnoreScannerParser parser;
    final ParserErrorListener lexerErrorListener;
    final ParserErrorListener parserErrorListener;
    final IgnoreDirectiveRecognizer directiveRecognizer;

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
        if (tree == null)
        {
            throw new Exception("Antlr-generated parser failed to accept input");
        }
        ParseTreeWalker walker = new ParseTreeWalker();
        Listener listener = new Listener(output, directiveRecognizer);
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
    }

    public RemoveIgnoreDirectiveFilter() throws Exception
    {
        lexer = new CLAWRemoveIgnoreScannerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new CLAWRemoveIgnoreScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
        directiveRecognizer = new IgnoreDirectiveRecognizer();
    }
}
