/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
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

public class FortranProcedureStatementsRecognizer
{
    enum Type {
        SubroutineOpen, SubroutineClose, FunctionOpen, FunctionClose
    };

    static class Listener extends FortranProcedureStatementsRecognizerBaseListener
    {
        final Type expectedType;
        public String unitName;

        public Exception error()
        {
            return _error;
        }

        Exception _error;

        public Listener(Type expectedType)
        {
            this.expectedType = expectedType;
        }

        void setUnitName(ParserRuleContext ctx, Type type)
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
            setUnitName(ctx, Type.FunctionOpen);
        }

        @Override
        public void exitFunction_close_name(FortranProcedureStatementsRecognizerParser.Function_close_nameContext ctx)
        {
            setUnitName(ctx, Type.FunctionClose);
        }

        @Override
        public void exitSubroutine_open_name(FortranProcedureStatementsRecognizerParser.Subroutine_open_nameContext ctx)
        {
            setUnitName(ctx, Type.SubroutineOpen);
        }

        @Override
        public void exitSubroutine_close_name(
                FortranProcedureStatementsRecognizerParser.Subroutine_close_nameContext ctx)
        {
            setUnitName(ctx, Type.SubroutineClose);
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

    Listener run(Type type, String input) throws IOException, Exception
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

    public String parseFunctionOpen(String input) throws IOException, Exception
    {
        return run(Type.FunctionOpen, input).unitName;
    }

    public String parseFunctionClose(String input) throws IOException, Exception
    {
        return run(Type.FunctionClose, input).unitName;
    }

    public String parseSubroutineOpen(String input) throws IOException, Exception
    {
        return run(Type.SubroutineOpen, input).unitName;
    }

    public String parseSubroutineClose(String input) throws IOException, Exception
    {
        return run(Type.SubroutineClose, input).unitName;
    }
}
