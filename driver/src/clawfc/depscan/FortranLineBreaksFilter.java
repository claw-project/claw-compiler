/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
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

import clawfc.depscan.FilteredContentSequence.Op;
import clawfc.depscan.FilteredContentSequence.OpType;
import clawfc.depscan.parser.FortranLineBreaksFilterBaseListener;
import clawfc.depscan.parser.FortranLineBreaksFilterLexer;
import clawfc.depscan.parser.FortranLineBreaksFilterParser;
import clawfc.utils.ByteArrayIOStream;

//FortranLineBreaksFilter works as follows:
//    1. Closed line breaks are removed 
//       Input:  "mod&\n   \n&ule"
//                   ---------
//       Output: "module" 
//    2. Unclosed line breaks are replaced with a single space
//    Input:  "module&\nm1" 
//                   ---
//    Output: "module m1" 
//    3. Optionally, newlines from line breaks are pushed in front of the broken line,
//       to preserve number of lines in the output text       
//       Input:  "mod&\n   \n&ul&  \n\n  &e"
//                   ---------  ----------
//                    x    x       x x
//                   
//       Output: "\n\n\n\nmodule"
public class FortranLineBreaksFilter
{
    static abstract class BasicListener extends FortranLineBreaksFilterBaseListener
    {
        final OutputStream outStrm;
        final ByteArrayIOStream outBuf;
        Integer outBufStartPos;

        public IOException error()
        {
            return _error;
        }

        IOException _error;

        public BasicListener(OutputStream outStrm)
        {
            _error = null;
            this.outStrm = outStrm;
            outBuf = new ByteArrayIOStream();
            outBufStartPos = null;
        }

        void writeError() throws CancellationException
        {
            throw new CancellationException("Write error");
        }

        protected void flushOutputBuf()
        {
            try
            {
                clawfc.Utils.copy(outBuf.getAsInputStreamUnsafe(), outStrm);
            } catch (IOException e)
            {
                _error = e;
                writeError();
            }
            outBuf.reset();
            outBufStartPos = null;
        }

        protected void addToOutputBuf(int pos, String s)
        {
            byte[] bytes = s.getBytes(StandardCharsets.US_ASCII);
            try
            {
                outBuf.write(bytes);
            } catch (IOException e)
            {
                _error = e;
                writeError();
            }
            if (outBufStartPos == null)
            {
                outBufStartPos = Integer.valueOf(pos);
            }
        }

        protected Integer getOutBufStartPos()
        {
            return outBufStartPos;
        }

        protected void output(String s)
        {
            byte[] bytes = s.getBytes(StandardCharsets.US_ASCII);
            try
            {
                outStrm.write(bytes);
            } catch (IOException e)
            {
                _error = e;
                writeError();
            }
        }
    }

    static class Listener extends BasicListener
    {
        final String lineBreakSep;

        final List<Op> ops;
        final List<Op> opsBuf;

        int linebreakEOLStartChrIdx;
        int numLinebreakEOLs;

        final List<Integer> lineBreakSubstLines;
        int outLineNum;

        final boolean preserveNumLines;

        public Listener(OutputStream outStrm, String lineBreakSep, boolean preserveNumLines)
        {
            super(outStrm);
            _error = null;
            this.lineBreakSep = lineBreakSep;
            ops = new ArrayList<Op>();
            opsBuf = new ArrayList<Op>();
            linebreakEOLStartChrIdx = -1;
            numLinebreakEOLs = 0;
            lineBreakSubstLines = new ArrayList<Integer>();
            outLineNum = 0;
            this.preserveNumLines = preserveNumLines;
        }

        @Override
        public void exitFortran_text(FortranLineBreaksFilterParser.Fortran_textContext ctx)
        {
            flushOutputBuf();
        }

        @Override
        public void exitOther(FortranLineBreaksFilterParser.OtherContext ctx)
        {
            String s = ctx.getText();
            int startChrIdx = Utils.getStartChrIdx(ctx);
            addToOutputBuf(startChrIdx, s);
        }

        @Override
        public void exitEol(FortranLineBreaksFilterParser.EolContext ctx)
        {
            flushOutputBuf();
            output("\n");
            ++outLineNum;
        }

        void bufferOutputLineBreakEOLs(String s, int startChrIdx)
        {
            for (char c : s.toCharArray())
            {
                if (c == '\n')
                {
                    if (numLinebreakEOLs == 0)
                    {
                        linebreakEOLStartChrIdx = startChrIdx;
                    }
                    ++numLinebreakEOLs;
                }
            }
        }

        @Override
        protected void flushOutputBuf()
        {
            if (numLinebreakEOLs > 0)
            {
                StringBuilder sB = new StringBuilder(numLinebreakEOLs);
                for (int i = 0; i < numLinebreakEOLs; ++i)
                {
                    sB.append('\n');
                    lineBreakSubstLines.add(outLineNum++);
                }
                numLinebreakEOLs = 0;
                String s = sB.toString();
                output(s);
                int idx = this.getOutBufStartPos() != null ? this.getOutBufStartPos() : linebreakEOLStartChrIdx;
                ops.add(new Op(OpType.ADD, idx, s));
            }
            ops.addAll(opsBuf);
            opsBuf.clear();
            super.flushOutputBuf();

        }

        @Override
        public void exitUnclosed_line_break(FortranLineBreaksFilterParser.Unclosed_line_breakContext ctx)
        {
            String text = ctx.getText();
            int startChrIdx = Utils.getStartChrIdx(ctx);
            if (preserveNumLines)
            {
                bufferOutputLineBreakEOLs(text, startChrIdx);
            }
            opsBuf.add(new Op(OpType.REMOVE, startChrIdx, text));
            addToOutputBuf(startChrIdx, lineBreakSep);
            opsBuf.add(new Op(OpType.ADD, startChrIdx, lineBreakSep));
        }

        @Override
        public void exitClosed_line_break(FortranLineBreaksFilterParser.Closed_line_breakContext ctx)
        {
            String text = ctx.getText();
            int startChrIdx = Utils.getStartChrIdx(ctx);
            if (preserveNumLines)
            {
                bufferOutputLineBreakEOLs(text, startChrIdx);
            }
            opsBuf.add(new Op(OpType.REMOVE, startChrIdx, text));
        }
    }

    FortranLineBreaksFilterLexer lexer;
    FortranLineBreaksFilterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranLineBreaksFilter() throws IOException
    {
        lexer = new FortranLineBreaksFilterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranLineBreaksFilterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    public FilteredContentSequence run(InputStream input, OutputStream output, boolean preserveNumLines,
            List<Integer> lineBreakSubstLines) throws FortranSyntaxException, IOException
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
        Listener listener = new Listener(output, " ", preserveNumLines);
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
        if (lineBreakSubstLines != null)
        {
            lineBreakSubstLines.clear();
            lineBreakSubstLines.addAll(listener.lineBreakSubstLines);
        }
        FilteredContentSequence res = FilteredContentSequence.decomposeIntoSeqs(listener.ops);
        return res;
    }

    public void run(InputStream input, OutputStream output) throws FortranSyntaxException, IOException
    {
        run(input, output, false, null);
    }
}
