/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import static clawfc.Utils.toInputStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ConsoleErrorListener;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import clawfc.depscan.FortranDepParser;
import clawfc.depscan.FortranDepScanner;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranFileBasicSummary;
import clawfc.depscan.FortranFileBuildInfo;
import clawfc.depscan.FortranFileBuildInfoDeserializer;
import clawfc.depscan.FortranFileBuildInfoSerializer;
import clawfc.depscan.FortranIncludesResolver;
import clawfc.depscan.FortranModuleBasicInfo;
import clawfc.depscan.FortranModuleInfo;
import clawfc.depscan.FortranSemanticException;
import clawfc.depscan.FortranStatementBasicPosition;
import clawfc.depscan.FortranStatementPosition;
import clawfc.depscan.FortranSyntaxException;
import clawfc.depscan.parser.FortranDepScannerBaseListener;
import clawfc.depscan.parser.FortranDepScannerLexer;
import clawfc.depscan.parser.FortranDepScannerParser;
import clawfc.utils.AsciiArrayIOStream;
import clawfc.utils.ByteArrayIOStream;
import junit.framework.TestCase;

class FortranDepScannerListener extends FortranDepScannerBaseListener
{
    public ArrayList<String> programOpen = new ArrayList<String>();
    public ArrayList<String> programClose = new ArrayList<String>();
    public ArrayList<String> moduleOpen = new ArrayList<String>();
    public ArrayList<String> moduleClose = new ArrayList<String>();
    public ArrayList<String> use = new ArrayList<String>();
    public ArrayList<String> other = new ArrayList<String>();

    @Override
    public void exitModule_open_stmt(FortranDepScannerParser.Module_open_stmtContext ctx)
    {
        moduleOpen.add(ctx.getText());
    }

    @Override
    public void exitModule_close_stmt(FortranDepScannerParser.Module_close_stmtContext ctx)
    {
        moduleClose.add(ctx.getText());
    }

    @Override
    public void exitProgram_open_stmt(FortranDepScannerParser.Program_open_stmtContext ctx)
    {
        programOpen.add(ctx.getText());
    }

    @Override
    public void exitProgram_close_stmt(FortranDepScannerParser.Program_close_stmtContext ctx)
    {
        programClose.add(ctx.getText());
    }

    @Override
    public void exitUse_stmt(FortranDepScannerParser.Use_stmtContext ctx)
    {
        use.add(ctx.getText());
    }
}

public class FortranDepScannerTest extends TestCase
{
    FortranDepScannerParser parser;
    FortranDepScannerLexer lexer;
    FortranDepParser depParser;
    FortranDepScanner depScanner;

    @Override
    protected void setUp() throws Exception
    {
        lexer = new FortranDepScannerLexer(toCharStream(""));
        lexer.removeErrorListener(ConsoleErrorListener.INSTANCE);
        parser = new FortranDepScannerParser(toTokenStream(""));
        parser.removeErrorListener(ConsoleErrorListener.INSTANCE);
        depParser = new FortranDepParser();
        depScanner = new FortranDepScanner();
    }

    private static CharStream toCharStream(String str) throws IOException
    {
        InputStream inStrm = new ByteArrayInputStream(str.getBytes(StandardCharsets.UTF_8));
        CharStream chrStrm = CharStreams.fromStream(inStrm, StandardCharsets.UTF_8);
        return chrStrm;
    }

    private CommonTokenStream toTokenStream(String str) throws IOException
    {
        CharStream chrStrm = toCharStream(str);
        lexer.reset();
        lexer.setInputStream(chrStrm);
        CommonTokenStream tokStrm = new CommonTokenStream(lexer);
        return tokStrm;
    }

    protected void acceptString(String str) throws IOException
    {
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.root();
        assertTrue(String.format("Failed to accept string \"%s\"", str),
                parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
    }

    protected void rejectString(String str) throws IOException
    {
        parser.reset();
        parser.setInputStream(toTokenStream(str));
        parser.root();
        assertTrue(String.format("Failed to reject string \"%s\"", str), parser.getNumberOfSyntaxErrors() > 0);
    }

    protected String flipChrCase(String str, int idx)
    {
        char c = str.charAt(idx);
        if (c >= 'a' && c <= 'z')
        {
            Character.toUpperCase(c);
        } else
        {
            Character.toLowerCase(c);
        }
        str = str.substring(0, idx) + c + str.substring(idx + 1);
        return str;
    }

    void acceptIdentifierString(String str) throws IOException
    {
        acceptString("module " + str + "\n end module x\n");
    }

    void rejectIdentifierString(String str) throws IOException
    {
        rejectString("module " + str + "\n end module x\n");
    }

    public void testIdentifier() throws Exception
    {

        for (char l = 'a'; l < 'z'; ++l)
        {
            acceptIdentifierString("" + l);
            acceptIdentifierString(l + "_");
            for (char l2 = '0'; l2 < '9'; ++l2)
            {
                acceptIdentifierString(("" + l) + l2);
            }
        }
        for (char l = 'A'; l < 'Z'; ++l)
        {
            acceptIdentifierString("" + l);
            acceptIdentifierString(l + "_");
            for (char l2 = '0'; l2 < '9'; ++l2)
            {
                acceptIdentifierString(("" + l) + l2);
            }
        }
        rejectIdentifierString("_");
        for (char l2 = '0'; l2 < '9'; ++l2)
        {
            rejectIdentifierString("" + l2);
        }
    }

    void acceptModuleOpenString(String str) throws IOException
    {
        acceptString(str + "\n end module x\n");
    }

    void rejectModuleOpenString(String str) throws IOException
    {
        rejectString(str + "\n end module x\n");
    }

    public void testModuleOpenStatement() throws Exception
    {
        acceptModuleOpenString("module module");
        acceptModuleOpenString("module x");
        acceptModuleOpenString(" module x");
        acceptModuleOpenString("\tmodule x");
        acceptModuleOpenString("\tmodule x");
        acceptModuleOpenString("module x ");
        acceptModuleOpenString("module x\t");
        acceptModuleOpenString("module x\t");
        for (int i = 0, n = "module".length(); i < n; ++i)
        {
            String str = flipChrCase("module", i) + " x";
            acceptModuleOpenString(str);
        }
        rejectModuleOpenString("module");
        rejectModuleOpenString("modulex");
        rejectModuleOpenString("module x !");
        rejectModuleOpenString("module x y");
    }

    void acceptModuleCloseString(String str) throws IOException
    {
        acceptString("module x\n" + str);
    }

    void rejectModuleCloseString(String str) throws IOException
    {
        rejectString("module x\n" + str);
    }

    public void testModuleCloseStatement() throws Exception
    {
        acceptModuleCloseString("end module x");
        acceptModuleCloseString(" end module x");
        acceptModuleCloseString("\tend module x");
        acceptModuleCloseString("\tend module x");
        acceptModuleCloseString("\tend\t\tmodule  x ");
        for (int i = 0, n = "end".length(); i < n; ++i)
        {
            String str = flipChrCase("end", i) + " module x";
            acceptModuleCloseString(str);
        }
        for (int i = 0, n = "module".length(); i < n; ++i)
        {
            String str = "end " + flipChrCase("module", i) + " x";
            acceptModuleCloseString(str);
        }
        rejectModuleCloseString("\tend\t\tmodule  x z");
    }

    void acceptProgramOpenString(String str) throws IOException
    {
        acceptString(str + "\n end program x\n");
    }

    void rejectProgramOpenString(String str) throws IOException
    {
        rejectString(str + "\n end program x\n");
    }

    public void testProgramOpenStatement() throws Exception
    {
        acceptProgramOpenString("program x");
        acceptProgramOpenString(" program x");
        acceptProgramOpenString("\tprogram x");
        acceptProgramOpenString("\tprogram x");
        acceptProgramOpenString("program x ");
        acceptProgramOpenString("program x\t");
        acceptProgramOpenString("program x\t");
        for (int i = 0, n = "program".length(); i < n; ++i)
        {
            String str = flipChrCase("program", i) + " x";
            acceptProgramOpenString(str);
        }
        rejectProgramOpenString("program");
        rejectProgramOpenString("programx");
        rejectProgramOpenString("program x !");
        rejectProgramOpenString("program x y");
    }

    void acceptProgramCloseString(String str) throws IOException
    {
        acceptString("program x\n" + str);
    }

    void rejectProgramCloseString(String str) throws IOException
    {
        rejectString("program x\n" + str);
    }

    public void testProgramCloseStatement() throws Exception
    {
        acceptProgramCloseString("end Program x");
        acceptProgramCloseString(" end Program x");
        acceptProgramCloseString("\tend Program x");
        acceptProgramCloseString("\tend Program x");
        acceptProgramCloseString("\tend\t\tProgram  xy_23");
        for (int i = 0, n = "end".length(); i < n; ++i)
        {
            String str = flipChrCase("end", i) + " Program x";
            acceptProgramCloseString(str);
        }
        for (int i = 0, n = "Program".length(); i < n; ++i)
        {
            String str = "end " + flipChrCase("Program", i) + " x";
            acceptProgramCloseString(str);
        }
        rejectProgramCloseString("\tend\t\tprogram x z");
    }

    void acceptUseString(String str) throws IOException
    {
        acceptString("program x\n" + str + "\nend program x\n");
    }

    void rejectUseString(String str) throws IOException
    {
        FortranDepScannerListener res = runParser("program x\n" + str + "\nend program x\n");
        assertEquals(1, res.programOpen.size());
        // "Unfit" use strings are classified as "other" and therefore skipped
        assertEquals(0, res.use.size());
        assertEquals(1, res.programClose.size());
        assertEquals("program x", res.programOpen.get(0));
        assertEquals("end program x", res.programClose.get(0));
    }

    public void testUseStatement() throws Exception
    {
        acceptUseString("use x");
        for (int i = 0, n = "use".length(); i < n; ++i)
        {
            String str = flipChrCase("use", i) + " x";
            acceptUseString(str);
        }
        acceptUseString(" use x");
        acceptUseString(" use x ");
        acceptUseString("use x,y1=>z1");
        acceptUseString("use x,  y1=>z1");
        acceptUseString("use x,  y1  =>z1");
        acceptUseString("use x,  y1  =>  z1");
        acceptUseString("use x,  y1  =>  z1  ");
        acceptUseString("use x,  y1  =>  z1, y2 => z2 ");
        acceptUseString("use x,  y1  =>  z1, y2 => z2, y3 => z3 ");
        acceptUseString("use x,  ONLY: y1");
        for (int i = 0, n = "only".length(); i < n; ++i)
        {
            String str = "use x,  " + flipChrCase("only", i) + ":x";
            acceptUseString(str);
        }
        acceptUseString("use x ,  ONLY : y1");
        acceptUseString("use x,  ONLY : y1");
        acceptUseString("use x,  ONLY: y1,y2");
        acceptUseString("use x,  ONLY: y1,y2,y3");
        acceptUseString("use x,  ONLY:y1=>z1");
        acceptUseString("use x,  ONLY: y1 =>z1");
        acceptUseString("use x,  ONLY: y1  =>z1");
        acceptUseString("use x,  ONLY: y1  =>  z1");
        acceptUseString("use x,  ONLY: y1  =>  z1  ");
        acceptUseString("use x,  ONLY: y1  =>  z1, y2 => z2, y3 => z3 ");
        rejectUseString("usex");
        rejectUseString("use x,");
        rejectUseString("use x y ");
        rejectUseString("use x, y,");
        rejectUseString("use x, ONLY ");
        rejectUseString("use x, ONLY: x y");
    }

    FortranDepScannerListener runParser(String inputStr) throws IOException
    {
        FortranDepScannerListener listener = new FortranDepScannerListener();
        parser.reset();
        parser.setInputStream(toTokenStream(inputStr));
        parser.setBuildParseTree(true);
        ParseTree tree = parser.root();
        ParseTreeWalker walker = new ParseTreeWalker();
        walker.walk(listener, tree);
        assertTrue(String.format("Failed to accept string \"%s\"", inputStr),
                parser.isMatchedEOF() && parser.getNumberOfSyntaxErrors() == 0);
        return listener;
    }

    void verifyProgramStatement(String s) throws Exception
    {
        FortranDepScannerListener res = runParser(s);
        assertEquals(1, res.programOpen.size());
        assertEquals(1, res.use.size());
        assertEquals(1, res.programClose.size());
        assertEquals("program x", res.programOpen.get(0));
        assertEquals("use y", res.use.get(0));
        assertEquals("end program x", res.programClose.get(0));
    }

    public void testProgramStatement() throws Exception
    {
        verifyProgramStatement("program x\n" + "use y\n" + "end program x\n");
        verifyProgramStatement("program x;use y\n" + "end program x\n");
        verifyProgramStatement("program x;use y;end program x;");
        verifyProgramStatement("\"program y;\";program x;use y\n" + "end program x\n");
        verifyProgramStatement("program x; \"program y;\"\nuse y\n" + "end program x\n");
    }

    public void testModuleStatement() throws Exception
    {
        FortranDepScannerListener res = runParser("module x\n" + "use y\n" + "end module x\n");
        assertEquals(1, res.moduleOpen.size());
        assertEquals(1, res.use.size());
        assertEquals(1, res.moduleClose.size());
        assertEquals("module x", res.moduleOpen.get(0));
        assertEquals("use y", res.use.get(0));
        assertEquals("end module x", res.moduleClose.get(0));
    }

    FortranFileBasicSummary parse(String s) throws IOException, FortranException, Exception
    {
        return depParser.parse(toInputStream(s));
    }

    void verifyParse(String s, List<FortranModuleBasicInfo> expModules, FortranModuleBasicInfo expectedProgram)
            throws IOException, FortranException, Exception
    {
        {
            FortranFileBasicSummary res = parse(s);
            FortranFileBasicSummary expRes = new FortranFileBasicSummary(expModules, expectedProgram);
            assertEquals(expRes, res);
        }
        {
            s = s.replace("\n", ";");
            FortranFileBasicSummary res = parse(s);
            FortranFileBasicSummary expRes = new FortranFileBasicSummary(expModules, expectedProgram);
            assertEquals(expRes, res);
        }
    }

    static FortranStatementBasicPosition BPos(String name, int startCharIdx, int endCharIdx)
    {
        return new FortranStatementBasicPosition(name, startCharIdx, endCharIdx);
    }

    public void testParsing() throws IOException, FortranException, Exception
    {
        verifyParse("", Arrays.asList(), (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + "end module\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 19), Arrays.asList())),
                (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + "end module \n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 19), Arrays.asList())),
                (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + "end module x\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 21), Arrays.asList())),
                (FortranModuleBasicInfo) null);
        verifyParse(" \t\tmodule x\n" + "end module x\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 3, 24), Arrays.asList())),
                (FortranModuleBasicInfo) null);
        verifyParse(" \t\tmodule x\n" + "end module x \t\t\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 3, 24), Arrays.asList())),
                (FortranModuleBasicInfo) null);
        verifyParse(" \t\tprogram x\n" + "end program x\n", Arrays.asList(),
                new FortranModuleBasicInfo(BPos("x", 3, 26), Arrays.asList()));
        verifyParse(" \t\tprogram x\n" + "end program x \t\t\n", Arrays.asList(),
                new FortranModuleBasicInfo(BPos("x", 3, 26), Arrays.asList()));
        verifyParse("module x\n" + "use y\n" + "end module x\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 27), Arrays.asList(BPos("y", 9, 14)))),
                (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + " \t\tuse y\n" + "end module x\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 30), Arrays.asList(BPos("y", 12, 17)))),
                (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + " \t\tuse y \t\t\n" + "end module x\n",
                Arrays.asList(new FortranModuleBasicInfo(BPos("x", 0, 33), Arrays.asList(BPos("y", 12, 17)))),
                (FortranModuleBasicInfo) null);
        verifyParse("module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n", Arrays.asList(
                new FortranModuleBasicInfo(BPos("x", 0, 37), Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24)))),
                (FortranModuleBasicInfo) null);
        verifyParse(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n",
                Arrays.asList(
                        new FortranModuleBasicInfo(BPos("x", 0, 37),
                                Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24))),
                        new FortranModuleBasicInfo(BPos("x1", 38, 79),
                                Arrays.asList(BPos("y1", 48, 54), BPos("z1", 59, 65)))),
                (FortranModuleBasicInfo) null);

        verifyParse(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n" + "program p1\n" + "use y1\n" + "bla\n" + "use z1\n"
                        + "end program p1\n",
                Arrays.asList(
                        new FortranModuleBasicInfo(BPos("x", 0, 37),
                                Arrays.asList(BPos("y", 9, 14), BPos("z", 19, 24))),
                        new FortranModuleBasicInfo(BPos("x1", 38, 79),
                                Arrays.asList(BPos("y1", 48, 54), BPos("z1", 59, 65)))),
                new FortranModuleBasicInfo(BPos("p1", 80, 123),
                        Arrays.asList(BPos("y1", 91, 97), BPos("z1", 102, 108))));

    }

    public void testSyntaxError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("end module x\n");
        } catch (FortranSyntaxException e)
        {
            assertTrue(e.getMessage().contains("extraneous input 'end module x'"));
            assertEquals(Integer.valueOf(0), e.getLineIndex());
            assertEquals(Integer.valueOf(0), e.getCharIdxInLine());
            assertEquals(null, e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    public void testModuleNamesMismatchError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("module x\n" + "end module y\n");
        } catch (FortranSemanticException e)
        {
            assertEquals("End module name \"y\" does not match current module name \"x\"", e.getMessage());
            assertEquals(null, e.getLineIndex());
            assertEquals(null, e.getCharIdxInLine());
            assertEquals(Integer.valueOf(9), e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    public void testDoubleDefError() throws IOException, FortranException, Exception
    {
        try
        {
            parse("module x\n" + "end module x\n" + "module x\n" + "end module x\n");
        } catch (FortranSemanticException e)
        {
            assertEquals("Double definition of module \"x\"", e.getMessage());
            assertEquals(null, e.getLineIndex());
            assertEquals(null, e.getCharIdxInLine());
            assertEquals(Integer.valueOf(22), e.getCharIdxInFile());
            return;
        }
        assertTrue(false);
    }

    FortranFileBasicSummary basicScan(String s) throws IOException, FortranException, Exception
    {
        return depScanner.basicScan(toInputStream(s));
    }

    FortranFileBuildInfo scan(String s) throws IOException, FortranException, Exception
    {
        return depScanner.scan(toInputStream(s));
    }

    void verifyBasicScan(String s, List<FortranModuleBasicInfo> expModules, FortranModuleBasicInfo expectedProgram)
            throws IOException, FortranException, Exception
    {
        FortranFileBasicSummary res = basicScan(s);
        FortranFileBasicSummary expRes = new FortranFileBasicSummary(expModules, expectedProgram);
        assertEquals(expRes, res);
    }

    void verifyScan(String s, List<FortranModuleInfo> expModules, FortranModuleInfo expectedProgram)
            throws IOException, FortranException, Exception
    {
        FortranFileBuildInfo res = scan(s);
        FortranFileBuildInfo expRes = new FortranFileBuildInfo(expModules, expectedProgram);
        assertEquals(expRes, res);
    }

    public static FortranStatementPosition Pos(String name, int startCharIdx, int endCharIdx, int startLineIdx,
            int endLineIdx)
    {
        return new FortranStatementPosition(name, startCharIdx, endCharIdx, startLineIdx, endLineIdx);
    }

    public void testScanning() throws IOException, FortranException, Exception
    {
        verifyScan("", Arrays.asList(), (FortranModuleInfo) null);
        verifyScan("module x\n" + "end module\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 19, 0, 2), Arrays.asList(), false)),
                (FortranModuleInfo) null);
        verifyScan("program x\n" + "end program\n", Arrays.asList(),
                new FortranModuleInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false));
        verifyScan("module x\n" + "end module \n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 19, 0, 2), Arrays.asList(), false)),
                (FortranModuleInfo) null);
        verifyScan("module x\n" + "end module x\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false)),
                (FortranModuleInfo) null);
        verifyScan("module x;" + "end module x;\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 21, 0, 1), Arrays.asList(), false)),
                (FortranModuleInfo) null);

        verifyScan("module x\n" + "use y\n" + "end module x\n",
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 27, 0, 3), Arrays.asList(Pos("y", 9, 14, 1, 2)), false)),
                (FortranModuleInfo) null);
        verifyScan("module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                        Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false)),
                (FortranModuleInfo) null);
        verifyScan(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n",
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranModuleInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)),
                (FortranModuleInfo) null);

        verifyScan(
                "module x\n" + "use y\n" + "bla\n" + "use z\n" + "end module x\n" + "module x1\n" + "use y1\n" + "bla\n"
                        + "use z1\n" + "end module x1\n" + "program p1\n" + "use y1\n" + "bla\n" + "use z1\n"
                        + "end program p1\n",
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranModuleInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)),
                new FortranModuleInfo(Pos("p1", 80, 123, 10, 15),
                        Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false));
    }

    public void testScanWithComments() throws IOException, FortranException, Exception
    {
        verifyScan("module x  ! comment1 \n" + "end module x ! comment2\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 34, 0, 2), Arrays.asList(), false)),
                (FortranModuleInfo) null);
        verifyScan("module x  ! comment1 \n" + "use z\n" + "end module x ! comment2\n",
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 40, 0, 3), Arrays.asList(Pos("z", 22, 27, 1, 2)), false)),
                (FortranModuleInfo) null);
        verifyScan("program x  ! comment1 \n" + "end program x ! comment2\n", Arrays.asList(),
                new FortranModuleInfo(Pos("x", 0, 36, 0, 2), Arrays.asList(), false));
    }

    public void testScanWithLineBreaks() throws IOException, FortranException, Exception
    {
        verifyScan("mo&  \n" + "&dule&\n" + "x\n" + "end module x\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 27, 0, 4), Arrays.asList(), false)),
                (FortranModuleInfo) null);
        verifyScan("pro&  \n" + "&gram&\n" + "x\n" + "end program x\n", Arrays.asList(),
                new FortranModuleInfo(Pos("x", 0, 29, 0, 4), Arrays.asList(), false));
    }

    public void testScanWithCommentsAndLineBreaks() throws IOException, FortranException, Exception
    {
        verifyScan("mod&   \n" + "   &ule x  ! comment1 \n" + "end mod&   \n" + "    &ule x ! comment2\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 53, 0, 4), Arrays.asList(), false)),
                (FortranModuleInfo) null);
    }

    public void testWithClawDirectives() throws Exception
    {
        verifyScan("module x\n" + "!$claw\n" + "end module x\n",
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 28, 0, 3), Arrays.asList(), true)),
                (FortranModuleInfo) null);
        verifyScan("program x\n" + "!$claw\n" + "end program x\n", Arrays.asList(),
                new FortranModuleInfo(Pos("x", 0, 30, 0, 3), Arrays.asList(), true));
    }

    protected final Path RES_DIR = clawfc.ut.Resources.DIR;

    public void testWithIncludes() throws Exception
    {
        final Path IN_DIR = RES_DIR.resolve("scan/include/input");
        final Path IN_FILEPATH = IN_DIR.resolve("2.f90");
        List<Path> incSearchPath = Arrays.asList(IN_DIR);
        AsciiArrayIOStream resOutStrm = new AsciiArrayIOStream();
        FortranFileBuildInfo res = depScanner.scan(Files.newInputStream(IN_FILEPATH), IN_FILEPATH, resOutStrm,
                incSearchPath);
        AsciiArrayIOStream refOutStrm = new AsciiArrayIOStream();
        FortranFileBuildInfo ref = null;
        Set<Path> refIncFiles = null;
        {
            FortranIncludesResolver resolver = new FortranIncludesResolver();
            refIncFiles = resolver.run(IN_FILEPATH, new AsciiArrayIOStream(IN_FILEPATH), refOutStrm, incSearchPath);
        }
        assertEquals(clawfc.Utils.collectIntoString(refOutStrm.getAsInputStreamUnsafe()),
                clawfc.Utils.collectIntoString(resOutStrm.getAsInputStreamUnsafe()));
        {
            FortranFileBuildInfo refBase = depScanner.scan(refOutStrm.getAsInputStreamUnsafe());
            ref = new FortranFileBuildInfo(refBase.getModules(), refBase.getProgram(),
                    new ArrayList<Path>(refIncFiles));
        }
        assertEquals(ref, res);
    }

    void verifySerialization(FortranFileBuildInfo obj) throws Exception
    {
        FortranFileBuildInfoSerializer serializer = new FortranFileBuildInfoSerializer();
        FortranFileBuildInfoDeserializer deserializer = new FortranFileBuildInfoDeserializer(true);
        ByteArrayIOStream buf = new ByteArrayIOStream();
        serializer.serialize(obj, buf);
        FortranFileBuildInfo deObj = deserializer.deserialize(buf.getAsInputStreamUnsafe());
        assertEquals(obj, deObj);
    }

    public void testSerialization() throws Exception
    {
        verifySerialization(new FortranFileBuildInfo(Arrays.asList(), null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 21, 0, 2), Arrays.asList(), false)), null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 21, 0, 1), Arrays.asList(), false)),
                (FortranModuleInfo) null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 27, 0, 3), Arrays.asList(Pos("y", 9, 14, 1, 2)), false)),
                (FortranModuleInfo) null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                        Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false)),
                (FortranModuleInfo) null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranModuleInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)),
                (FortranModuleInfo) null));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranModuleInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)),
                new FortranModuleInfo(Pos("p1", 80, 123, 10, 15),
                        Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false),
                Paths.get("/tmp/bla-dir/bla.file")));
        verifySerialization(new FortranFileBuildInfo(
                Arrays.asList(
                        new FortranModuleInfo(Pos("x", 0, 37, 0, 5),
                                Arrays.asList(Pos("y", 9, 14, 1, 2), Pos("z", 19, 24, 3, 4)), false),
                        new FortranModuleInfo(Pos("x1", 38, 79, 5, 10),
                                Arrays.asList(Pos("y1", 48, 54, 6, 7), Pos("z1", 59, 65, 8, 9)), false)),
                new FortranModuleInfo(Pos("p1", 80, 123, 10, 15),
                        Arrays.asList(Pos("y1", 91, 97, 11, 12), Pos("z1", 102, 108, 13, 14)), false),
                Paths.get("/tmp/bla-dir/bla.file"), Arrays.asList(Paths.get("/inc_dir1"), Paths.get("inc_dir2"))));
    }
}
