/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranProcedureStatementsRecognizerBaseListener;
import clawfc.depscan.parser.FortranProcedureStatementsRecognizerLexer;
import clawfc.depscan.parser.FortranProcedureStatementsRecognizerParser;

public class FortranProcedureStatementsRecognizer implements FortranProgramUnitStatementsRecognizer
{
    public static final Set<StatementType> SUPPORTED_TYPES = new HashSet<StatementType>(
            Arrays.asList(StatementType.FunctionOpen, StatementType.FunctionClose, StatementType.SubroutineOpen,
                    StatementType.SubroutineClose));

    static class Listener extends FortranProcedureStatementsRecognizerBaseListener
    {
        final StatementType expectedType;
        public String unitName;

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
        public void exitFunction_open_name(FortranProcedureStatementsRecognizerParser.Function_open_nameContext ctx)
        {
            setUnitName(ctx, StatementType.FunctionOpen);
        }

        @Override
        public void exitFunction_close_name(FortranProcedureStatementsRecognizerParser.Function_close_nameContext ctx)
        {
            setUnitName(ctx, StatementType.FunctionClose);
        }

        @Override
        public void exitSubroutine_open_name(FortranProcedureStatementsRecognizerParser.Subroutine_open_nameContext ctx)
        {
            setUnitName(ctx, StatementType.SubroutineOpen);
        }

        @Override
        public void exitSubroutine_close_name(
                FortranProcedureStatementsRecognizerParser.Subroutine_close_nameContext ctx)
        {
            setUnitName(ctx, StatementType.SubroutineClose);
        }
    }

    FortranProcedureStatementsRecognizerLexer lexer;
    clawfc.depscan.parser.FortranProcedureStatementsRecognizerParser parser;

    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranProcedureStatementsRecognizer() throws IOException
    {
        lexer = new FortranProcedureStatementsRecognizerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new clawfc.depscan.parser.FortranProcedureStatementsRecognizerParser(new CommonTokenStream(lexer));
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
            case FunctionOpen:
                tree = parser.function_open_stmt();
                break;
            case FunctionClose:
                tree = parser.function_close_stmt();
                break;
            case SubroutineOpen:
                tree = parser.subroutine_open_stmt();
                break;
            case SubroutineClose:
                tree = parser.subroutine_close_stmt();
                break;
            default:
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

    @Override
    public String parseUnitStatement(StatementType type, String input) throws IOException, Exception
    {
        final String unitName = run(type, input).unitName;
        return unitName != null ? unitName.toLowerCase() : null;
    }
}
