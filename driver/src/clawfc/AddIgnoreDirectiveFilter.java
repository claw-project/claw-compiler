/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc;

import static clawfc.Utils.getEolStartIndex;
import static clawfc.Utils.sprintf;
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

import clawfc.parsers.CLAWAddIgnoreScannerBaseListener;
import clawfc.parsers.CLAWAddIgnoreScannerLexer;
import clawfc.parsers.CLAWAddIgnoreScannerParser;
import clawfc.utils.ParserErrorListener;

/**
 * <code>AddIgnoreDirectiveFilter</code> wraps ANTLR-generated parser for
 * decorating Fortran code with CLAW ignore.
 *
 */
public class AddIgnoreDirectiveFilter
{
    static class Listener extends CLAWAddIgnoreScannerBaseListener
    {
        OutputStream output;
        Integer ignoreBlockStartLineNum;

        public Exception getError()
        {
            return error;
        }

        public Integer getIgnoreBlockStartLineNum()
        {
            return ignoreBlockStartLineNum;
        }

        Exception error;

        public Listener(OutputStream output)
        {
            this.output = output;
            ignoreBlockStartLineNum = null;
            error = null;
        }

        void onError(Exception e)
        {
            error = e;
            throw new CancellationException();
        }

        @Override
        public void exitClaw_ignore_start_line(CLAWAddIgnoreScannerParser.Claw_ignore_start_lineContext ctx)
        {
            try
            {
                if (ignoreBlockStartLineNum == null)
                {
                    ignoreBlockStartLineNum = Integer.valueOf(ctx.getStart().getLine());
                    output.write(toAscii(ctx.getText()));
                } else
                {
                    throw new Exception(sprintf("Unclosed ignore directive at line %s", ignoreBlockStartLineNum));
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitClaw_ignore_end_line(CLAWAddIgnoreScannerParser.Claw_ignore_end_lineContext ctx)
        {
            try
            {
                if (ignoreBlockStartLineNum != null)
                {
                    ignoreBlockStartLineNum = null;
                    output.write(toAscii(ctx.getText()));
                } else
                {
                    int lineNum = ctx.getStart().getLine();
                    String errMsg = sprintf("\"Ignore end\" directive at line %s with no ignore block open", lineNum);
                    throw new Exception(errMsg);
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitOther_line(CLAWAddIgnoreScannerParser.Other_lineContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                final String line;
                if (ignoreBlockStartLineNum != null)
                {
                    final int eolIdx = getEolStartIndex(txt);
                    line = sprintf("!$claw ignore |%s|%s", txt.substring(0, eolIdx), txt.substring(eolIdx));
                } else
                {
                    line = txt;
                }
                output.write(toAscii(line));
            } catch (Exception e)
            {
                onError(e);
            }
        }
    }

    CLAWAddIgnoreScannerLexer lexer;
    CLAWAddIgnoreScannerParser parser;
    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

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
        Listener listener = new Listener(output);
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
        if (listener.getIgnoreBlockStartLineNum() != null)
        {
            final int lineNum = listener.getIgnoreBlockStartLineNum();
            throw new Exception(sprintf("Unclosed ignore directive at line %s", lineNum));
        }
    }

    public AddIgnoreDirectiveFilter() throws Exception
    {
        lexer = new CLAWAddIgnoreScannerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new CLAWAddIgnoreScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
