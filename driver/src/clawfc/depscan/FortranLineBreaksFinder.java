/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

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

import clawfc.depscan.parser.FortranLineBreaksFilterBaseListener;
import clawfc.depscan.parser.FortranLineBreaksFilterLexer;
import clawfc.depscan.parser.FortranLineBreaksFilterParser;

public class FortranLineBreaksFinder
{
    public static class LineBreakInfo
    {
        public final int startIdx;
        public final int endIdx;
        public final boolean closed;

        public LineBreakInfo(int startIdx, int endIdx, boolean closed)
        {
            this.startIdx = startIdx;
            this.endIdx = endIdx;
            this.closed = closed;
        }

        @Override
        public boolean equals(Object obj)
        {
            if (this == obj)
            {
                return true;
            }
            if (obj == null)
            {
                return false;
            }
            if (getClass() != obj.getClass())
            {
                return false;
            }
            LineBreakInfo other = (LineBreakInfo) obj;
            if (startIdx != other.startIdx)
            {
                return false;
            }
            if (endIdx != other.endIdx)
            {
                return false;
            }
            if (closed != other.closed)
            {
                return false;
            }
            return true;
        }
    }

    static class Listener extends FortranLineBreaksFilterBaseListener
    {
        final List<LineBreakInfo> lineBreaks;

        public IOException error()
        {
            return _error;
        }

        IOException _error;

        public Listener()
        {
            _error = null;
            this.lineBreaks = new ArrayList<LineBreakInfo>();
        }

        @Override
        public void exitUnclosed_line_break(FortranLineBreaksFilterParser.Unclosed_line_breakContext ctx)
        {
            String text = ctx.getText();
            int startChrIdx = Utils.getStartChrIdx(ctx);
            int endChrIdx = startChrIdx + text.length();
            lineBreaks.add(new LineBreakInfo(startChrIdx, endChrIdx, false));
        }

        @Override
        public void exitClosed_line_break(FortranLineBreaksFilterParser.Closed_line_breakContext ctx)
        {
            String text = ctx.getText();
            int startChrIdx = Utils.getStartChrIdx(ctx);
            int endChrIdx = startChrIdx + text.length();
            lineBreaks.add(new LineBreakInfo(startChrIdx, endChrIdx, true));
        }
    }

    FortranLineBreaksFilterLexer lexer;
    FortranLineBreaksFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranLineBreaksFinder() throws IOException
    {
        lexer = new FortranLineBreaksFilterLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranLineBreaksFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    public List<LineBreakInfo> run(InputStream input) throws FortranSyntaxException, IOException
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
        return listener.lineBreaks;
    }
}
