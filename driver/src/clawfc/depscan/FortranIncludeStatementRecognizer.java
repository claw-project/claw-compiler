/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranIncludeStatementRecognizerBaseListener;
import clawfc.depscan.parser.FortranIncludeStatementRecognizerLexer;
import clawfc.depscan.parser.FortranIncludeStatementRecognizerParser;

public class FortranIncludeStatementRecognizer
{
    static class Listener extends FortranIncludeStatementRecognizerBaseListener
    {
        String includeString;

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
        public void exitInclude_string(FortranIncludeStatementRecognizerParser.Include_stringContext ctx)
        {
            String txt = ctx.getText();
            includeString = getFortranStringContents(txt);
        }
    }

    FortranIncludeStatementRecognizerLexer lexer;
    clawfc.depscan.parser.FortranIncludeStatementRecognizerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranIncludeStatementRecognizer() throws IOException
    {
        lexer = new FortranIncludeStatementRecognizerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new clawfc.depscan.parser.FortranIncludeStatementRecognizerParser(new CommonTokenStream(lexer));
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
        CharStream chrStrm = toCharStream(input);
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
        return listener.includeString;
    }
}
