/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
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
        public LinkedHashMap<String, LinkedHashSet<String>> moduleDependencies;
        public LinkedHashMap<String, LinkedHashSet<String>> programDependencies;
        public Map<String, Integer> moduleLineStart;
        public Map<String, Integer> moduleLineEnd;
        public Map<String, Integer> programLineStart;
        public Map<String, Integer> programLineEnd;

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
            moduleDependencies = new LinkedHashMap<String, LinkedHashSet<String>>();
            programDependencies = new LinkedHashMap<String, LinkedHashSet<String>>();
            moduleLineStart = new HashMap<String, Integer>();
            moduleLineEnd = new HashMap<String, Integer>();
            programLineStart = new HashMap<String, Integer>();
            programLineEnd = new HashMap<String, Integer>();
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

        @Override
        public void exitModule_open_stmt(FortranDepScannerParser.Module_open_stmtContext ctx)
        {
            try
            {
                currentModuleName = statementsParser.parseModuleOpen(ctx.getText()).moduleOpenName;
            } catch (Exception e)
            {
                onError(e);
            }
            if (!moduleDependencies.containsKey(currentModuleName))
            {
                moduleDependencies.put(currentModuleName, new LinkedHashSet<String>());
                int currentModuleStartLine = getLineNum(ctx.start);
                moduleLineStart.put(currentModuleName, currentModuleStartLine);
            } else
            {
                String errMsg = String.format("Double definition of module \"%s\"", currentModuleName);
                int lineNum = ctx.start.getLine();
                onError(new FortranSemanticException(errMsg, lineNum, 0));
            }
        }

        @Override
        public void exitModule_close_stmt(FortranDepScannerParser.Module_close_stmtContext ctx)
        {
            String moduleName = null;
            try
            {
                moduleName = statementsParser.parseModuleClose(ctx.getText()).moduleCloseName;
            } catch (Exception e)
            {
                onError(e);
            }
            if (moduleName.equals(currentModuleName))
            {
                int currentModuleEndLine = getLineNum(ctx.start);
                moduleLineEnd.put(currentModuleName, currentModuleEndLine);
                currentModuleName = null;
            } else
            {
                String errMsg = String.format("End module name \"%s\" does not match current module name \"%s\"",
                        moduleName, currentModuleName);
                int lineNum = ctx.start.getLine();
                onError(new FortranSemanticException(errMsg, lineNum, 0));
            }
        }

        @Override
        public void exitProgram_open_stmt(FortranDepScannerParser.Program_open_stmtContext ctx)
        {
            try
            {
                currentProgramName = statementsParser.parseProgramOpen(ctx.getText()).programOpenName;
            } catch (Exception e)
            {
                onError(e);
            }
            if (!programDependencies.containsKey(currentProgramName))
            {
                if (programDependencies.isEmpty())
                {
                    programDependencies.put(currentProgramName, new LinkedHashSet<String>());
                    int currentProgramStartLine = getLineNum(ctx.start);
                    programLineStart.put(currentProgramName, currentProgramStartLine);
                } else
                {
                    String firstProgramName = programDependencies.entrySet().iterator().next().getKey();
                    String errMsg = String.format("Another program \"%s\" already defined before \"%s\"",
                            firstProgramName, currentProgramName);
                    int lineNum = ctx.start.getLine();
                    onError(new FortranSemanticException(errMsg, lineNum, 0));
                }
            } else
            {
                String errMsg = String.format("Double definition of Program \"%s\"", currentProgramName);
                int lineNum = ctx.start.getLine();
                onError(new FortranSemanticException(errMsg, lineNum, 0));
            }
        }

        @Override
        public void exitProgram_close_stmt(FortranDepScannerParser.Program_close_stmtContext ctx)
        {
            String programName = null;
            try
            {
                programName = statementsParser.parseProgramClose(ctx.getText()).programCloseName;
            } catch (Exception e)
            {
                onError(e);
            }
            if (programName.equals(currentProgramName))
            {
                int currentProgramEndLine = getLineNum(ctx.start);
                programLineEnd.put(currentProgramName, currentProgramEndLine);
                currentProgramName = null;
            } else
            {
                String errMsg = String.format("End program name \"%s\" does not match current program name \"%s\"",
                        programName, currentProgramName);
                int lineNum = ctx.start.getLine();
                onError(new FortranSemanticException(errMsg, lineNum, 0));
            }
        }

        @Override
        public void exitUse_stmt(FortranDepScannerParser.Use_stmtContext ctx)
        {
            String useModuleName = null;
            try
            {
                useModuleName = statementsParser.parseUse(ctx.getText()).useModuleName;
            } catch (Exception e)
            {
                onError(e);
            }
            if (currentModuleName != null)
            {
                moduleDependencies.get(currentModuleName).add(useModuleName);
            } else if (currentProgramName != null)
            {
                programDependencies.get(currentProgramName).add(useModuleName);
            }
        }
    }

    FortranDepStatementsRecognizer statementsParser;
    FortranDepScannerLexer lexer;
    FortranDepScannerParser parser;
    ParserErrorListener lexerErrorListener;
    ParserErrorListener parserErrorListener;

    public FortranFileBasicSummary parse(InputStream input, List<Integer> lineBreakSubstLines)
            throws FortranException, IOException, Exception
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

        if (lineBreakSubstLines != null && !lineBreakSubstLines.isEmpty())
        {
            adjustStartsForLinebreaks(listener.moduleLineStart, lineBreakSubstLines);
            adjustStartsForLinebreaks(listener.programLineStart, lineBreakSubstLines);
        }

        FortranFileBasicSummary res = new FortranFileBasicSummary(listener.moduleDependencies, listener.moduleLineStart,
                listener.moduleLineEnd, listener.programDependencies, listener.programLineStart,
                listener.programLineEnd);
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
        lexer = new FortranDepScannerLexer(Utils.toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        lexerErrorListener = new ParserErrorListener();
        lexer.addErrorListener(lexerErrorListener);
        parser = new FortranDepScannerParser(new CommonTokenStream(lexer));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parserErrorListener = new ParserErrorListener();
        parser.addErrorListener(parserErrorListener);
    }
}
