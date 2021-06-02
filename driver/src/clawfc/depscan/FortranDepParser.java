/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;
import static java.lang.String.format;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.FortranDepParser.Listener.FileStructureParser;
import clawfc.depscan.FortranProgramUnitStatementsRecognizer.StatementType;
import clawfc.depscan.parser.FortranDepScannerLexer;
import clawfc.depscan.parser.FortranDepScannerParser;
import clawfc.depscan.serial.FortranProgramUnitType;

/**
 * <code>FortranDepParser</code> wraps ANTLR-generated parsers for recognizing
 * Fortran language constructs related to dependencies (module, use etc.).
 *
 */
public class FortranDepParser
{
    static String DEFAULT_PROGRAM_NAME = "_unnamed_program";
    static String DEFAULT_BLOCKDATA_NAME = "_unnamed_blockdata";

    public static interface StatementInfoVisitor
    {
        void visit(CloseStatementInfo info) throws FortranSemanticException;

        void visit(OpenStatementInfo info) throws FortranSemanticException;

        void visit(UseModuleStatementInfo info) throws FortranSemanticException;
    }

    public static abstract class StatementInfo
    {
        protected StatementInfo(FortranProgramUnitType type, int startChrIdx, int endChrIdx, String name)
        {
            this.type = type;
            this.startChrIdx = startChrIdx;
            this.endChrIdx = endChrIdx;
            this.name = name;
        }

        public final FortranProgramUnitType type;
        public final int startChrIdx;
        public final int endChrIdx;
        public final String name;

        abstract void accept(StatementInfoVisitor visitor) throws FortranSemanticException;
    }

    public static class OpenStatementInfo extends StatementInfo
    {
        public OpenStatementInfo(FortranProgramUnitType type, int startChrIdx, int endChrIdx, String name)
        {
            super(type, startChrIdx, endChrIdx, name);
        }

        @Override
        public void accept(StatementInfoVisitor visitor) throws FortranSemanticException
        {
            visitor.visit(this);
        }

    }

    public static class CloseStatementInfo extends StatementInfo
    {
        public CloseStatementInfo(FortranProgramUnitType type, int startChrIdx, int endChrIdx, String name)
        {
            super(type, startChrIdx, endChrIdx, name);
        }

        @Override
        public void accept(StatementInfoVisitor visitor) throws FortranSemanticException
        {
            visitor.visit(this);
        }
    }

    public static class UseModuleStatementInfo extends StatementInfo
    {
        public UseModuleStatementInfo(int startChrIdx, int endChrIdx, String name)
        {
            super(null, startChrIdx, endChrIdx, name);
        }

        @Override
        public void accept(StatementInfoVisitor visitor) throws FortranSemanticException
        {
            visitor.visit(this);
        }
    }

    static class Listener extends clawfc.depscan.parser.FortranDepScannerBaseListener
    {
        FortranDepStatementsRecognizer statementsParser;
        FortranProcedureStatementsRecognizer procStatementsParser;

        public List<StatementInfo> statements;

        public Exception error()
        {
            return _error;
        }

        Exception _error;
        String currentModuleName;
        String currentProgramName;
        int lineNum;

        public Listener(FortranDepStatementsRecognizer statementsParser,
                FortranProcedureStatementsRecognizer procStatementsParser)
        {
            _error = null;
            this.statementsParser = statementsParser;
            this.procStatementsParser = procStatementsParser;
            statements = new ArrayList<StatementInfo>();
        }

        void onError(Exception e)
        {
            _error = e;
            throw new CancellationException();
        }

        static int getLineNum(org.antlr.v4.runtime.Token token)
        {// Line numbers are counted from zero, unless used in error messages
            return token.getLine() - 1;
        }

        int numPrefWhitespaces(String s)
        {
            for (int i = 0, n = s.length(); i < n; ++i)
            {
                if (!Character.isWhitespace(s.charAt(i)))
                {
                    return i;
                }
            }
            return 0;
        }

        int numSufWhitespaces(String s)
        {
            int n = 0;
            for (int i = s.length() - 1; i >= 0; --i, ++n)
            {
                if (!Character.isWhitespace(s.charAt(i)))
                {
                    return n;
                }
            }
            return n;
        }

        FortranProgramUnitStatementsRecognizer getRecognizer(StatementType type)
        {
            if (FortranDepStatementsRecognizer.SUPPORTED_TYPES.contains(type))
            {
                return statementsParser;
            } else if (FortranProcedureStatementsRecognizer.SUPPORTED_TYPES.contains(type))
            {
                return procStatementsParser;
            } else
            {
                return null;
            }
        }

        void parseOpenStatement(final StatementType type, final ParserRuleContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                FortranProgramUnitStatementsRecognizer recognizer = getRecognizer(type);
                final String unitName = recognizer.parseUnitStatement(type, txt);
                int currentUnitStartChrIdx = Utils.getStartChrIdx(ctx) + numPrefWhitespaces(txt);
                final FortranProgramUnitType unitType = type.toUnitType();
                statements.add(new OpenStatementInfo(unitType, currentUnitStartChrIdx, -1, unitName));
            } catch (Exception e)
            {
                onError(e);
            }
        }

        void parseCloseStatement(final StatementType type, final ParserRuleContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                FortranProgramUnitStatementsRecognizer recognizer = getRecognizer(type);
                final String unitName = recognizer.parseUnitStatement(type, txt);
                final int exitUnitStartChrIdx = Utils.getStartChrIdx(ctx);
                final int exitUnitEndChrIdx = exitUnitStartChrIdx + txt.length() - numSufWhitespaces(txt);
                final FortranProgramUnitType unitType = type.toUnitType();
                statements.add(new CloseStatementInfo(unitType, exitUnitStartChrIdx, exitUnitEndChrIdx, unitName));

            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitModule_open_stmt(FortranDepScannerParser.Module_open_stmtContext ctx)
        {
            parseOpenStatement(StatementType.MODULE_OPEN, ctx);
        }

        @Override
        public void exitModule_close_stmt(FortranDepScannerParser.Module_close_stmtContext ctx)
        {
            parseCloseStatement(StatementType.MODULE_CLOSE, ctx);
        }

        @Override
        public void exitProgram_open_stmt(FortranDepScannerParser.Program_open_stmtContext ctx)
        {
            parseOpenStatement(StatementType.PROGRAM_OPEN, ctx);
        }

        @Override
        public void exitProgram_close_stmt(FortranDepScannerParser.Program_close_stmtContext ctx)
        {
            parseCloseStatement(StatementType.PROGRAM_CLOSE, ctx);
        }

        @Override
        public void exitBlock_data_open_stmt(FortranDepScannerParser.Block_data_open_stmtContext ctx)
        {
            parseOpenStatement(StatementType.BLOCK_DATA_OPEN, ctx);
        }

        @Override
        public void exitBlock_data_close_stmt(FortranDepScannerParser.Block_data_close_stmtContext ctx)
        {
            parseCloseStatement(StatementType.BLOCK_DATA_CLOSE, ctx);
        }

        @Override
        public void exitFunction_open_stmt(FortranDepScannerParser.Function_open_stmtContext ctx)
        {
            parseOpenStatement(StatementType.FUNCTION_OPEN, ctx);
        }

        @Override
        public void exitFunction_close_stmt(FortranDepScannerParser.Function_close_stmtContext ctx)
        {
            parseCloseStatement(StatementType.FUNCTION_CLOSE, ctx);
        }

        @Override
        public void exitSubroutine_open_stmt(FortranDepScannerParser.Subroutine_open_stmtContext ctx)
        {
            parseOpenStatement(StatementType.SUBROUTINE_OPEN, ctx);
        }

        @Override
        public void exitSubroutine_close_stmt(FortranDepScannerParser.Subroutine_close_stmtContext ctx)
        {
            parseCloseStatement(StatementType.SUBROUTINE_CLOSE, ctx);
        }

        @Override
        public void exitProgram_unit_close_stmt(FortranDepScannerParser.Program_unit_close_stmtContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                final int exitUnitStartChrIdx = Utils.getStartChrIdx(ctx);
                final int exitUnitEndChrIdx = exitUnitStartChrIdx + txt.length() - numSufWhitespaces(txt);
                statements.add(new CloseStatementInfo(null, exitUnitStartChrIdx, exitUnitEndChrIdx, null));
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitUse_stmt(FortranDepScannerParser.Use_stmtContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                String useModuleName = statementsParser.parseUse(txt).moduleName;
                int numPrefWS = numPrefWhitespaces(txt);
                int numSufWS = numSufWhitespaces(txt);
                int startIdx = Utils.getStartChrIdx(ctx);
                final int useModuleStartChrIdx = startIdx + numPrefWS;
                final int useModuleEndChrIdx = startIdx + txt.length() - numSufWS;
                statements.add(new UseModuleStatementInfo(useModuleStartChrIdx, useModuleEndChrIdx, useModuleName));
            } catch (Exception e)
            {
                onError(e);
            }
        }

        static class FileStructureParser implements StatementInfoVisitor
        {
            List<FortranProgramUnitBasicInfo> units = new ArrayList<FortranProgramUnitBasicInfo>();
            Map<String, FortranProgramUnitType> unitTypeByName = new HashMap<String, FortranProgramUnitType>();
            Deque<OpenStatementInfo> unitOpenStack = new ArrayDeque<OpenStatementInfo>();
            List<UseModuleStatementInfo> modIncludes = new ArrayList<UseModuleStatementInfo>();

            List<FortranProgramUnitBasicInfo> getUnits()
            {
                return units;
            }

            boolean isProcedure(FortranProgramUnitType type)
            {
                return type == FortranProgramUnitType.FUNCTION || type == FortranProgramUnitType.SUBROUTINE;
            }

            boolean subprogramAllowed()
            {
                return true;
                /*-Check disabled because of currently unsupported INTERFACE block, which
                can contain procedure declarations
                final int unitStackSize = unitOpenStack.size();
                if (unitStackSize == 1)
                {
                    return true;
                } else if (unitStackSize == 2)
                {
                    final FortranProgramUnitType topUnitType = unitOpenStack.getLast().type;
                    if (topUnitType == FortranProgramUnitType.MODULE)
                    {
                        return true;
                    } else
                    {
                        return false;
                    }
                } else
                {
                    return false;
                }*/
            }

            @Override
            public void visit(OpenStatementInfo info) throws FortranSemanticException
            {
                if (unitOpenStack.isEmpty())
                {
                    unitOpenStack.push(info);
                } else
                {
                    final FortranProgramUnitType type = info.type;
                    final FortranProgramUnitType lastType = unitOpenStack.peek().type;
                    if (subprogramAllowed())
                    {
                        if (lastType != FortranProgramUnitType.BLOCK_DATA)
                        {
                            if (isProcedure(type))
                            {// Only subprograms can be contained
                                unitOpenStack.push(info);
                            } else
                            {
                                throw new FortranSemanticException(
                                        format("%s statement is not allowed inside %s", type, lastType),
                                        info.startChrIdx);
                            }
                        } else
                        {
                            throw new FortranSemanticException(
                                    format("%s statement is not allowed inside %s", type, lastType), info.startChrIdx);
                        }
                    } else
                    {
                        throw new FortranSemanticException(
                                format("%s statement is not allowed inside subprogram %s", type, lastType),
                                info.startChrIdx);
                    }
                }
            }

            List<FortranStatementBasicPosition> getUseModPositions()
            {
                Map<String, FortranStatementBasicPosition> useModPosSet = new LinkedHashMap<String, FortranStatementBasicPosition>();
                for (UseModuleStatementInfo info : modIncludes)
                {
                    useModPosSet.put(info.name,
                            new FortranStatementBasicPosition(info.name, info.startChrIdx, info.endChrIdx));
                }
                List<FortranStatementBasicPosition> useModPositions = new ArrayList<FortranStatementBasicPosition>(
                        useModPosSet.size());
                for (Map.Entry<String, FortranStatementBasicPosition> useEntry : useModPosSet.entrySet())
                {
                    useModPositions.add(useEntry.getValue());
                }
                modIncludes.clear();
                return Collections.unmodifiableList(useModPositions);
            }

            void addUnit(OpenStatementInfo openInfo, CloseStatementInfo closeInfo) throws FortranSemanticException
            {
                final FortranProgramUnitType type;
                final int startChrIdx;
                final String name;
                if (openInfo == null)
                {// Unnamed program
                    type = FortranProgramUnitType.PROGRAM;
                    if (units.isEmpty())
                    {
                        startChrIdx = 0;
                    } else
                    {
                        startChrIdx = units.get(units.size() - 1).getPosition().getEndCharIdx();
                    }
                    name = DEFAULT_PROGRAM_NAME;
                } else
                {
                    type = openInfo.type;
                    startChrIdx = openInfo.startChrIdx;
                    if (openInfo.name != null)
                    {
                        name = openInfo.name;
                    } else
                    {
                        name = DEFAULT_BLOCKDATA_NAME;
                    }
                }
                final int endChrIdx = closeInfo.endChrIdx;
                final FortranProgramUnitType prevType = unitTypeByName.get(name);
                if (prevType == null)
                {
                    unitTypeByName.put(name, type);
                    final FortranStatementBasicPosition pos = new FortranStatementBasicPosition(name, startChrIdx,
                            endChrIdx);
                    final List<FortranStatementBasicPosition> useModPositions = getUseModPositions();
                    units.add(new FortranProgramUnitBasicInfo(type, pos, useModPositions));
                } else
                {
                    throw new FortranSemanticException(
                            format("%s \"%s\" has the same name as prior %s", type, name, prevType), startChrIdx);
                }
            }

            @Override
            public void visit(CloseStatementInfo closeInfo) throws FortranSemanticException
            {
                if (!unitOpenStack.isEmpty())
                {
                    final OpenStatementInfo openInfo = unitOpenStack.peek();
                    final FortranProgramUnitType lastType = openInfo.type;
                    if (closeInfo.type == lastType || closeInfo.type == null)
                    {
                        final String openName = openInfo.name;
                        final String closeName = closeInfo.name;
                        if (closeName == null || openName.equals(closeName))
                        {
                            unitOpenStack.pop();
                            if (unitOpenStack.isEmpty())
                            {// Only top level units are considered program units
                                addUnit(openInfo, closeInfo);
                            }

                        } else
                        {
                            throw new FortranSemanticException(
                                    format("%s open name \"%s\" does not match close name \"%s\"", lastType, openName,
                                            closeName),
                                    closeInfo.startChrIdx);
                        }
                    } else
                    {
                        throw new FortranSemanticException(
                                format("%s open closed by mismatched %s end", lastType, closeInfo.type),
                                closeInfo.startChrIdx);
                    }
                } else
                {
                    if (closeInfo.type == null)
                    {
                        addUnit(null, closeInfo);
                    } else
                    {
                        if (closeInfo.name != null)
                        {
                            throw new FortranSemanticException(
                                    format("Unmatched END %s %s", closeInfo.type, closeInfo.name),
                                    closeInfo.startChrIdx);
                        } else
                        {
                            throw new FortranSemanticException(format("Unmatched END %s", closeInfo.type),
                                    closeInfo.startChrIdx);
                        }
                    }
                }

            }

            @Override
            public void visit(UseModuleStatementInfo info) throws FortranSemanticException
            {
                if (!unitOpenStack.isEmpty())
                {
                    modIncludes.add(info);
                } else
                {
                    throw new FortranSemanticException("USE statement can only appear inside program unit",
                            info.startChrIdx);
                }
            }
        }
    }

    final FortranDepStatementsRecognizer statementsParser;
    final FortranProcedureStatementsRecognizer procStatementsParser;
    final FortranDepScannerLexer lexer;
    final FortranDepScannerParser parser;
    final ParserErrorListener lexerErrorListener;
    final ParserErrorListener parserErrorListener;

    public static FortranFileBasicSummary getSummary(final List<StatementInfo> statements) throws Exception
    {
        FileStructureParser summaryCreator = new FileStructureParser();
        for (final StatementInfo stmtInfo : statements)
        {
            stmtInfo.accept(summaryCreator);
        }
        final FortranFileBasicSummary summary = new FortranFileBasicSummary(summaryCreator.getUnits());
        return summary;
    }

    public List<StatementInfo> parse(InputStream input) throws FortranException, IOException, Exception
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
        Listener listener = new Listener(statementsParser, procStatementsParser);
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
        return listener.statements;
    }

    public FortranDepParser() throws Exception
    {
        statementsParser = new FortranDepStatementsRecognizer();
        procStatementsParser = new FortranProcedureStatementsRecognizer();
        lexer = new FortranDepScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranDepScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
