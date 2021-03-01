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
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.PreprocessorOutputSplitterBaseListener;
import clawfc.depscan.parser.PreprocessorOutputSplitterLexer;
import clawfc.depscan.parser.PreprocessorOutputSplitterParser;
import clawfc.depscan.parser.PreprocessorOutputSplitterParser.Non_preproc_lineContext;

/**
 * Removes preprocessor-added lines from the input. Extracts include files paths
 * from line markers.
 */
/**
 * @author mike
 *
 */
public class PreprocessorOutputScanner
{
    static class Listener extends PreprocessorOutputSplitterBaseListener
    {
        Exception _error;
        Set<Path> includes;
        final OutputStream nonPPOutput;
        final PreprocessorLineMarkerRecognizer recognizer;

        public Exception error()
        {
            return _error;
        }

        public Set<Path> getIncludes()
        {
            return includes;
        }

        public Listener(OutputStream nonPPOutput) throws IOException
        {
            _error = null;
            includes = new LinkedHashSet<Path>();
            recognizer = new PreprocessorLineMarkerRecognizer();
            this.nonPPOutput = nonPPOutput;
        }

        @Override
        public void exitNon_preproc_line(Non_preproc_lineContext ctx)
        {
            String txt = ctx.getText();
            byte[] bytes = txt.getBytes(StandardCharsets.US_ASCII);
            try
            {
                nonPPOutput.write(bytes);
            } catch (IOException e)
            {
                _error = e;
                throw new CancellationException("Write error");
            }

        }

        @Override
        public void exitPreproc_line_marker_line(PreprocessorOutputSplitterParser.Preproc_line_marker_lineContext ctx)
        {
            String txt = ctx.getText();
            try
            {
                String filePathStr = recognizer.run(txt);
                Path filePath = Paths.get(filePathStr);
                if (filePath.isAbsolute())
                {
                    includes.add(filePath.normalize());
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException("PreprocessorLineMarkerRecognizer failed");
            }
        }
    }

    PreprocessorOutputSplitterLexer lexer;
    PreprocessorOutputSplitterParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    /**
     * @throws IOException
     */
    public PreprocessorOutputScanner() throws IOException
    {
        lexer = new PreprocessorOutputSplitterLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new PreprocessorOutputSplitterParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    /**
     * Only absolute paths are recognized, so the preprocessor should be given only
     * absolute paths to include directories and source file.
     */
    public Set<Path> run(InputStream input, OutputStream nonPPOutput) throws Exception
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
        Listener listener = new Listener(nonPPOutput);
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
