/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.toCharStream;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CancellationException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.parser.FortranDepScannerBaseListener;
import clawfc.depscan.parser.FortranDepScannerLexer;
import clawfc.depscan.parser.FortranDepScannerParser;

public class FortranDepParser
{
    static class Listener extends FortranDepScannerBaseListener
    {
        FortranDepStatementsRecognizer statementsParser;

        class StmtPos
        {
            public final int chrStartIdx;
            public final int chrEndIdx;

            public StmtPos(int chrStartIdx, int chrEndIdx)
            {
                this.chrStartIdx = chrStartIdx;
                this.chrEndIdx = chrEndIdx;
            }
        }

        public LinkedHashMap<String, LinkedHashMap<String, StmtPos>> moduleDependencies;
        public LinkedHashMap<String, LinkedHashMap<String, StmtPos>> programDependencies;
        public Map<String, Integer> moduleStmtStartChrIdx;
        public Map<String, Integer> moduleStmtEndChrIdx;
        public Map<String, Integer> programStmtStartChrIdx;
        public Map<String, Integer> programStmtEndChrIdx;

        public Exception error()
        {
            return _error;
        }

        Exception _error;
        String currentModuleName;
        String currentProgramName;
        int lineNum;

        public Listener(FortranDepStatementsRecognizer statementsParser)
        {
            _error = null;
            this.statementsParser = statementsParser;
            moduleDependencies = new LinkedHashMap<String, LinkedHashMap<String, StmtPos>>();
            programDependencies = new LinkedHashMap<String, LinkedHashMap<String, StmtPos>>();
            moduleStmtStartChrIdx = new HashMap<String, Integer>();
            moduleStmtEndChrIdx = new HashMap<String, Integer>();
            programStmtStartChrIdx = new HashMap<String, Integer>();
            programStmtEndChrIdx = new HashMap<String, Integer>();
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

        @Override
        public void exitModule_open_stmt(FortranDepScannerParser.Module_open_stmtContext ctx)
        {
            try
            {
                String txt = ctx.getText();
                currentModuleName = statementsParser.parseModuleOpen(txt);
                int currentModuleStartChrIdx = Utils.getStartChrIdx(ctx) + numPrefWhitespaces(txt);
                if (!moduleDependencies.containsKey(currentModuleName))
                {
                    moduleDependencies.put(currentModuleName, new LinkedHashMap<String, StmtPos>());
                    moduleStmtStartChrIdx.put(currentModuleName, currentModuleStartChrIdx);
                } else
                {
                    String errMsg = String.format("Double definition of module \"%s\"", currentModuleName);
                    throw new FortranSemanticException(errMsg, currentModuleStartChrIdx, null, null);
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitModule_close_stmt(FortranDepScannerParser.Module_close_stmtContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                final String moduleName = statementsParser.parseModuleClose(txt);
                final int exitModuleStartChrIdx = Utils.getStartChrIdx(ctx);
                final int exitModuleEndChrIdx = exitModuleStartChrIdx + txt.length() - numSufWhitespaces(txt);
                if (moduleName == null || moduleName.equals(currentModuleName))
                {
                    moduleStmtEndChrIdx.put(currentModuleName, exitModuleEndChrIdx);
                    currentModuleName = null;
                } else
                {
                    String errMsg = String.format("End module name \"%s\" does not match current module name \"%s\"",
                            moduleName, currentModuleName);
                    throw new FortranSemanticException(errMsg, exitModuleStartChrIdx, null, null);
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitProgram_open_stmt(FortranDepScannerParser.Program_open_stmtContext ctx)
        {
            try
            {
                String txt = ctx.getText();
                currentProgramName = statementsParser.parseProgramOpen(txt);
                final int currentProgramStartIdx = Utils.getStartChrIdx(ctx) + numPrefWhitespaces(txt);
                if (!programDependencies.containsKey(currentProgramName))
                {
                    if (programDependencies.isEmpty())
                    {
                        programDependencies.put(currentProgramName, new LinkedHashMap<String, StmtPos>());
                        programStmtStartChrIdx.put(currentProgramName, currentProgramStartIdx);
                    } else
                    {
                        String firstProgramName = programDependencies.entrySet().iterator().next().getKey();
                        String errMsg = String.format("Another program \"%s\" already defined before \"%s\"",
                                firstProgramName, currentProgramName);
                        throw new FortranSemanticException(errMsg, currentProgramStartIdx, null, null);
                    }
                } else
                {
                    String errMsg = String.format("Double definition of Program \"%s\"", currentProgramName);
                    throw new FortranSemanticException(errMsg, currentProgramStartIdx, null, null);
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        @Override
        public void exitProgram_close_stmt(FortranDepScannerParser.Program_close_stmtContext ctx)
        {
            try
            {
                final String txt = ctx.getText();
                final String programName = statementsParser.parseProgramClose(txt);
                final int exitProgramStartChrIdx = Utils.getStartChrIdx(ctx);
                final int exitProgramEndChrIdx = exitProgramStartChrIdx + txt.length() - numSufWhitespaces(txt);
                if (programName == null || programName.equals(currentProgramName))
                {
                    programStmtEndChrIdx.put(currentProgramName, exitProgramEndChrIdx);
                    currentProgramName = null;
                } else
                {
                    String errMsg = String.format("End program name \"%s\" does not match current program name \"%s\"",
                            programName, currentProgramName);
                    throw new FortranSemanticException(errMsg, exitProgramStartChrIdx, null, null);
                }
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
                final StmtPos pos = new StmtPos(useModuleStartChrIdx, useModuleEndChrIdx);
                if (currentModuleName != null)
                {
                    moduleDependencies.get(currentModuleName).put(useModuleName, pos);
                } else if (currentProgramName != null)
                {
                    programDependencies.get(currentProgramName).put(useModuleName, pos);
                }
            } catch (Exception e)
            {
                onError(e);
            }
        }

        static List<FortranModuleBasicInfo> getModInfo(
                LinkedHashMap<String, LinkedHashMap<String, StmtPos>> moduleDependencies,
                Map<String, Integer> moduleStmtStartChrIdx, Map<String, Integer> moduleStmtEndChrIdx)
        {
            ArrayList<FortranModuleBasicInfo> modules = new ArrayList<FortranModuleBasicInfo>(
                    moduleDependencies.size());
            for (Map.Entry<String, LinkedHashMap<String, StmtPos>> entry : moduleDependencies.entrySet())
            {
                String moduleName = entry.getKey();
                int startChrIdx = moduleStmtStartChrIdx.get(moduleName);
                int endChrIdx = moduleStmtEndChrIdx.get(moduleName);
                FortranStatementBasicPosition modPos = new FortranStatementBasicPosition(moduleName, startChrIdx,
                        endChrIdx);
                List<FortranStatementBasicPosition> useModPositions = new ArrayList<FortranStatementBasicPosition>();
                for (Map.Entry<String, StmtPos> useEntry : entry.getValue().entrySet())
                {
                    String useModName = useEntry.getKey();
                    int useModStartChrIdx = useEntry.getValue().chrStartIdx;
                    int useModEndChrIdx = useEntry.getValue().chrEndIdx;
                    useModPositions
                            .add(new FortranStatementBasicPosition(useModName, useModStartChrIdx, useModEndChrIdx));
                }
                useModPositions = Collections.unmodifiableList(useModPositions);
                modules.add(new FortranModuleBasicInfo(modPos, Collections.unmodifiableList(useModPositions)));
            }
            return Collections.unmodifiableList(modules);
        }

        public FortranFileBasicSummary getSummary()
        {
            List<FortranModuleBasicInfo> modules = getModInfo(moduleDependencies, moduleStmtStartChrIdx,
                    moduleStmtEndChrIdx);
            List<FortranModuleBasicInfo> programs = getModInfo(programDependencies, programStmtStartChrIdx,
                    programStmtEndChrIdx);
            FortranFileBasicSummary summary = new FortranFileBasicSummary(modules,
                    !programs.isEmpty() ? programs.get(0) : null);
            return summary;
        }
    }

    FortranDepStatementsRecognizer statementsParser;
    FortranDepScannerLexer lexer;
    FortranDepScannerParser parser;
    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranFileBasicSummary parse(InputStream input) throws FortranException, IOException, Exception
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
        Listener listener = new Listener(statementsParser);
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
        FortranFileBasicSummary res = listener.getSummary();
        return res;
    }

    void adjustStartsForLinebreaks(Map<String, Integer> startLines, List<Integer> lineBreakSubstLines)
    {
        Map<Integer, Integer> offsetByEndIdx = compressLineNumsIntoRanges(lineBreakSubstLines);
        for (Map.Entry<String, Integer> entry : startLines.entrySet())
        {
            int startLineNum = entry.getValue();
            Integer offset = offsetByEndIdx.get(startLineNum - 1);
            if (offset != null)
            {
                startLineNum -= offset;
                entry.setValue(startLineNum);
            }
        }
    }

    Map<Integer, Integer> compressLineNumsIntoRanges(List<Integer> lineBreakSubstLines)
    {
        Map<Integer, Integer> rangesByEnd = new HashMap<Integer, Integer>();
        int prev = -2;
        int n = 0;
        for (Integer i : lineBreakSubstLines)
        {
            if (n > 0 && ((prev + 1) == i))
            {
                prev = i;
                ++n;
            } else
            {
                if (n > 0)
                {
                    rangesByEnd.put(prev, n);
                }
                prev = i;
                n = 1;
            }
        }
        if (n > 0)
        {
            rangesByEnd.put(prev, n);
        }
        return rangesByEnd;
    }

    public FortranDepParser() throws Exception
    {
        statementsParser = new FortranDepStatementsRecognizer();
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
