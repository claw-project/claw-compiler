/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.util.ArrayList;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranDepStatementsRecognizerBaseListener;
import clawfc.depscan.parser.FortranDepStatementsRecognizerLexer;
import clawfc.depscan.parser.FortranDepStatementsRecognizerParser;

public class FortranDepStatementsRecognizer
{
    public static class RenamedSymbol
    {
        public RenamedSymbol(String from, String to)
        {
            this.from = from;
            this.to = to;
        }

        public String from;
        public String to;
    }

    public static class Data
    {
        public String moduleOpenName;
        public String moduleCloseName;
        public String programOpenName;
        public String programCloseName;
        public String useModuleName;
        public boolean useOnly = false;
        public ArrayList<String> useSymbols = new ArrayList<String>();
        public ArrayList<RenamedSymbol> useRenamedSymbols = new ArrayList<RenamedSymbol>();
    }

    static class Listener extends FortranDepStatementsRecognizerBaseListener
    {
        public Data data = new Data();

        String useSymName;
        String useOnlySymName;

        @Override
        public void exitModule_open_name(FortranDepStatementsRecognizerParser.Module_open_nameContext ctx)
        {
            data.moduleOpenName = ctx.getText();
        }

        @Override
        public void exitModule_close_name(FortranDepStatementsRecognizerParser.Module_close_nameContext ctx)
        {
            data.moduleCloseName = ctx.getText();
        }

        @Override
        public void exitProgram_open_name(FortranDepStatementsRecognizerParser.Program_open_nameContext ctx)
        {
            data.programOpenName = ctx.getText();
        }

        @Override
        public void exitProgram_close_name(FortranDepStatementsRecognizerParser.Program_close_nameContext ctx)
        {
            data.programCloseName = ctx.getText();
        }

        @Override
        public void exitUse_module_name(FortranDepStatementsRecognizerParser.Use_module_nameContext ctx)
        {
            data.useModuleName = ctx.getText();
        }

        @Override
        public void enterOnly_list_stmt(FortranDepStatementsRecognizerParser.Only_list_stmtContext ctx)
        {
            data.useOnly = true;
        }

        @Override
        public void exitUse_symbol_name(FortranDepStatementsRecognizerParser.Use_symbol_nameContext ctx)
        {
            useSymName = ctx.getText();
        }

        @Override
        public void exitUse_symbol_name_from(FortranDepStatementsRecognizerParser.Use_symbol_name_fromContext ctx)
        {
            String from = ctx.getText();
            data.useRenamedSymbols.add(new RenamedSymbol(from, useSymName));
        }

        @Override
        public void exitUse_only_symbol_name(FortranDepStatementsRecognizerParser.Use_only_symbol_nameContext ctx)
        {
            useOnlySymName = ctx.getText();
        }

        @Override
        public void exitUse_only_symbol_name_from(
                FortranDepStatementsRecognizerParser.Use_only_symbol_name_fromContext ctx)
        {
            String from = ctx.getText();
            data.useRenamedSymbols.add(new RenamedSymbol(from, useOnlySymName));
            useOnlySymName = null;
        }

        @Override
        public void exitOnly_stmt(FortranDepStatementsRecognizerParser.Only_stmtContext ctx)
        {
            if (useOnlySymName != null)
            {
                data.useSymbols.add(useOnlySymName);
            }
        }
    }

    FortranDepStatementsRecognizerLexer lexer;
    clawfc.depscan.parser.FortranDepStatementsRecognizerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranDepStatementsRecognizer() throws IOException
    {
        lexer = new FortranDepStatementsRecognizerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new clawfc.depscan.parser.FortranDepStatementsRecognizerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }

    Data run(String input, Callable<ParseTree> parseStatement) throws IOException, Exception
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
            tree = parseStatement.call();
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
        return listener.data;
    }

    public Data parseModuleOpen(String input) throws IOException, Exception
    {
        return run(input, () -> {
            return parser.module_open_stmt();
        });
    }

    public Data parseModuleClose(String input) throws IOException, Exception
    {
        return run(input, () -> {
            return parser.module_close_stmt();
        });
    }

    public Data parseProgramOpen(String input) throws IOException, Exception
    {
        return run(input, () -> {
            return parser.program_open_stmt();
        });
    }

    public Data parseProgramClose(String input) throws IOException, Exception
    {
        return run(input, () -> {
            return parser.program_close_stmt();
        });
    }

    public Data parseUse(String input) throws IOException, Exception
    {
        return run(input, () -> {
            return parser.use_stmt();
        });
    }
}
