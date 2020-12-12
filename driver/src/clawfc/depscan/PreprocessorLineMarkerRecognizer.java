/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.PreprocessorLineMarkerRecognizerBaseListener;
import clawfc.depscan.parser.PreprocessorLineMarkerRecognizerLexer;
import clawfc.depscan.parser.PreprocessorLineMarkerRecognizerParser.Filename_stringContext;

public class PreprocessorLineMarkerRecognizer
{
    static class Listener extends PreprocessorLineMarkerRecognizerBaseListener
    {
        String filePathString;

        String getFortranStringContents(String txt)
        {
            if (txt.startsWith("\""))
            {
                return txt.substring(1, txt.length() - 1).replace("\"\"", "\"");
            } else
            {
                return txt.substring(1, txt.length() - 1).replace("''", "'");
            }
        }

        @Override
        public void exitFilename_string(Filename_stringContext ctx)
        {
            filePathString = ctx.getText();
        }
    }

    PreprocessorLineMarkerRecognizerLexer lexer;
    clawfc.depscan.parser.PreprocessorLineMarkerRecognizerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public PreprocessorLineMarkerRecognizer() throws IOException
    {
        lexer = new PreprocessorLineMarkerRecognizerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new clawfc.depscan.parser.PreprocessorLineMarkerRecognizerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    public String run(String input) throws IOException, Exception
    {
        lexer.reset();
        parser.reset();
        lexerErrorListener.reset();
        parserErrorListener.reset();
        CharStream chrStrm = Utils.toCharStream(input);
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
        walker.walk(listener, tree);
        return listener.filePathString;
    }
}
