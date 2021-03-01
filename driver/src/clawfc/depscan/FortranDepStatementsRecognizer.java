/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranDepStatementsRecognizerBaseListener;
import clawfc.depscan.parser.FortranDepStatementsRecognizerLexer;
import clawfc.depscan.parser.FortranDepStatementsRecognizerParser;

public class FortranDepStatementsRecognizer implements FortranProgramUnitStatementsRecognizer
{
    public static final Set<StatementType> SUPPORTED_TYPES = new HashSet<StatementType>(Arrays.asList(
            StatementType.BlockDataOpen, StatementType.BlockDataClose, StatementType.ModuleOpen,
            StatementType.ModuleClose, StatementType.ProgramOpen, StatementType.ProgramClose, StatementType.UseModule));

    public static class RenamedSymbol
    {
        public RenamedSymbol(String from, String to)
        {
            this.from = from;
            this.to = to;
        }

        public final String from;
        public final String to;
    }

    public static class UseModuleData
    {
        public UseModuleData(String moduleName, boolean useOnly, List<String> useSymbols,
                List<RenamedSymbol> useRenamedSymbols)
        {
            this.moduleName = moduleName;
            this.useOnly = useOnly;
            this.useSymbols = Collections.unmodifiableList(useSymbols);
            this.useRenamedSymbols = Collections.unmodifiableList(useRenamedSymbols);
        }

        public final String moduleName;
        public final boolean useOnly;
        public final List<String> useSymbols;
        public final List<RenamedSymbol> useRenamedSymbols;
    }

    static class Listener extends FortranDepStatementsRecognizerBaseListener
    {
        final StatementType expectedType;
        public String unitName;
        public String moduleName;
        public boolean useOnly = false;
        public List<String> useSymbols = new ArrayList<String>();
        public List<RenamedSymbol> useRenamedSymbols = new ArrayList<RenamedSymbol>();

        String useSymName;
        String useOnlySymName;

        public Exception error()
        {
            return _error;
        }

        Exception _error;

        public Listener(StatementType expectedType)
        {
            this.expectedType = expectedType;
        }

        void setUnitName(ParserRuleContext ctx, StatementType type)
        {
            try
            {
                if (expectedType == type)
                {
                    unitName = ctx.getText();
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitModule_open_name(FortranDepStatementsRecognizerParser.Module_open_nameContext ctx)
        {
            setUnitName(ctx, StatementType.ModuleOpen);
        }

        @Override
        public void exitModule_close_name(FortranDepStatementsRecognizerParser.Module_close_nameContext ctx)
        {
            setUnitName(ctx, StatementType.ModuleClose);
        }

        @Override
        public void exitProgram_open_name(FortranDepStatementsRecognizerParser.Program_open_nameContext ctx)
        {
            setUnitName(ctx, StatementType.ProgramOpen);
        }

        @Override
        public void exitProgram_close_name(FortranDepStatementsRecognizerParser.Program_close_nameContext ctx)
        {
            setUnitName(ctx, StatementType.ProgramClose);
        }

        @Override
        public void exitBlock_data_open_name(FortranDepStatementsRecognizerParser.Block_data_open_nameContext ctx)
        {
            setUnitName(ctx, StatementType.BlockDataOpen);
        }

        @Override
        public void exitBlock_data_close_name(FortranDepStatementsRecognizerParser.Block_data_close_nameContext ctx)
        {
            setUnitName(ctx, StatementType.BlockDataClose);
        }

        @Override
        public void exitUse_module_name(FortranDepStatementsRecognizerParser.Use_module_nameContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    moduleName = ctx.getText();
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void enterOnly_list_stmt(FortranDepStatementsRecognizerParser.Only_list_stmtContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    useOnly = true;
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitUse_symbol_name(FortranDepStatementsRecognizerParser.Use_symbol_nameContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    useSymName = ctx.getText();
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitUse_symbol_name_from(FortranDepStatementsRecognizerParser.Use_symbol_name_fromContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    String from = ctx.getText();
                    useRenamedSymbols.add(new RenamedSymbol(from, useSymName));
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitUse_only_symbol_name(FortranDepStatementsRecognizerParser.Use_only_symbol_nameContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    useOnlySymName = ctx.getText();
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitUse_only_symbol_name_from(
                FortranDepStatementsRecognizerParser.Use_only_symbol_name_fromContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    String from = ctx.getText();
                    useRenamedSymbols.add(new RenamedSymbol(from, useOnlySymName));
                    useOnlySymName = null;
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
            }
        }

        @Override
        public void exitOnly_stmt(FortranDepStatementsRecognizerParser.Only_stmtContext ctx)
        {
            try
            {
                if (expectedType == StatementType.UseModule)
                {
                    if (useOnlySymName != null)
                    {
                        useSymbols.add(useOnlySymName);
                    }
                } else
                {
                    throw new Exception("Expected type mismatch");
                }
            } catch (Exception e)
            {
                _error = e;
                throw new CancellationException();
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

    Listener run(StatementType type, String input) throws IOException, Exception
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
            switch (type)
            {
            case ModuleOpen:
                tree = parser.module_open_stmt();
                break;
            case ModuleClose:
                tree = parser.module_close_stmt();
                break;
            case ProgramOpen:
                tree = parser.program_open_stmt();
                break;
            case ProgramClose:
                tree = parser.program_close_stmt();
                break;
            case BlockDataOpen:
                tree = parser.block_data_open_stmt();
                break;
            case BlockDataClose:
                tree = parser.block_data_close_stmt();
                break;
            case UseModule:
                tree = parser.use_stmt();
                break;
            }
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
        Listener listener = new Listener(type);
        walker.walk(listener, tree);
        if (listener.error() != null)
        {
            throw listener.error();
        }
        return listener;
    }

    public String parseUnitStatement(StatementType type, String input) throws IOException, Exception
    {
        final String unitName = run(type, input).unitName;
        return unitName != null ? unitName.toLowerCase() : null;
    }

    public UseModuleData parseUse(String input) throws IOException, Exception
    {
        Listener l = run(StatementType.UseModule, input);
        return new UseModuleData(l.moduleName.toLowerCase(), l.useOnly, l.useSymbols, l.useRenamedSymbols);
    }
}
