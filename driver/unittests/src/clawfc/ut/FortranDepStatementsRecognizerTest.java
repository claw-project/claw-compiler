/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.IOException;
import java.util.List;

import clawfc.Utils;
import clawfc.depscan.FortranDepParser;
import clawfc.depscan.FortranDepParser.CloseStatementInfo;
import clawfc.depscan.FortranDepParser.OpenStatementInfo;
import clawfc.depscan.FortranDepParser.StatementInfo;
import clawfc.depscan.FortranDepParser.UseModuleStatementInfo;
import clawfc.depscan.FortranDepStatementsRecognizer;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranProgramUnitStatementsRecognizer.StatementType;
import junit.framework.TestCase;

/**
 * FortranDepStatementsRecognizer is used only in tandem with
 * FortranDepStatementsScanner.
 */
public class FortranDepStatementsRecognizerTest extends TestCase
{
    private FortranDepStatementsRecognizer parser;
    private FortranDepParser scanner;

    @Override
    protected void setUp() throws Exception
    {
        parser = new FortranDepStatementsRecognizer();
        scanner = new FortranDepParser();
    }

    public void verifyProgramUnitStatementScan(final StatementType refType, final String line, final String refName)
            throws FortranException, IOException, Exception
    {
        List<StatementInfo> statements = scanner.parse(Utils.toInputStream(line + "\n"));
        assertEquals(1, statements.size());
        StatementInfo info = statements.get(0);
        if (info instanceof OpenStatementInfo)
        {
            final StatementType resType = StatementType.getOpenType(info.type);
            assertEquals(refType, resType);
            assertEquals(refName, ((OpenStatementInfo) info).name);
        } else if (info instanceof CloseStatementInfo)
        {
            final StatementType resType = StatementType.getCloseType(info.type);
            assertEquals(refType, resType);
            assertEquals(refName, ((CloseStatementInfo) info).name);
        } else
        {
            throw new Exception("Unexpected result");
        }
    }

    public void verifyUseModuleStatementScan(final String line, final String refName)
            throws FortranException, IOException, Exception
    {
        List<StatementInfo> statements = scanner.parse(Utils.toInputStream(line + "\n"));
        assertEquals(1, statements.size());
        StatementInfo info = statements.get(0);
        assertTrue(info instanceof UseModuleStatementInfo);
        UseModuleStatementInfo resInfo = (UseModuleStatementInfo) info;
        assertEquals(refName, resInfo.name);
    }

    void acceptModuleOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.ModuleOpen, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.ModuleOpen, line, name);
    }

    void rejectLine(String line) throws FortranException, IOException, Exception
    {
        List<StatementInfo> statements = scanner.parse(Utils.toInputStream(line + "\n"));
        assertTrue(statements.isEmpty());
    }

    public void testModuleOpen() throws Exception
    {
        acceptModuleOpen("module a", "a");
        acceptModuleOpen("module x", "x");
        acceptModuleOpen("module x", "x");
        acceptModuleOpen(" module x", "x");
        acceptModuleOpen("\tmodule x", "x");
        acceptModuleOpen("\tmodule x", "x");
        acceptModuleOpen("module x ", "x");
        acceptModuleOpen("module x\t", "x");
        acceptModuleOpen("module x\t", "x");
        for (int i = 0, n = "module".length(); i < n; ++i)
        {
            final String str = flipChrCase("module", i) + " x";
            acceptModuleOpen(str, "x");
        }
        acceptModuleOpen("module \t\txyz_123  \t", "xyz_123");
        rejectLine("module");
        rejectLine("modulex");
        rejectLine("module x !");
        rejectLine("module x y");
    }

    void acceptModuleClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.ModuleClose, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.ModuleClose, line, name);
    }

    public void testModuleClose() throws Exception
    {
        acceptModuleClose("endmodule x", "x");
        acceptModuleClose("end module x", "x");
        acceptModuleClose("end\tmodule \t\txyz_123  \t", "xyz_123");
        acceptModuleClose("end\tmodule", null);
        acceptModuleClose("end\tmodule\t", null);
        for (int i = 0, n = "end".length(); i < n; ++i)
        {
            String str = flipChrCase("end", i) + " module x";
            acceptModuleClose(str, "x");
        }
        for (int i = 0, n = "module".length(); i < n; ++i)
        {
            String str = "end " + flipChrCase("module", i) + " x";
            acceptModuleClose(str, "x");
        }
    }

    void acceptProgramOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.ProgramOpen, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.ProgramOpen, line, name);
    }

    public void testProgramOpen() throws Exception
    {
        acceptProgramOpen("program x", "x");
        acceptProgramOpen(" program x", "x");
        acceptProgramOpen("\tprogram x", "x");
        acceptProgramOpen("\tprogram x", "x");
        acceptProgramOpen("program x ", "x");
        acceptProgramOpen("program x\t", "x");
        acceptProgramOpen("program x\t", "x");
        for (int i = 0, n = "program".length(); i < n; ++i)
        {
            String str = flipChrCase("program", i) + " x";
            acceptProgramOpen(str, "x");
        }
        acceptProgramOpen("program x", "x");
        acceptProgramOpen("program \t\txyz_123  \t", "xyz_123");
        rejectLine("program");
        rejectLine("programx");
        rejectLine("program x !");
        rejectLine("program x y");
    }

    void acceptProgramClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.ProgramClose, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.ProgramClose, line, name);
    }

    public void testProgramClose() throws Exception
    {
        acceptProgramClose("end Program x", "x");
        acceptProgramClose(" end Program x", "x");
        acceptProgramClose("\tend Program x", "x");
        acceptProgramClose("\tend Program x", "x");
        acceptProgramClose("\tend\t\tProgram  xy_23", "xy_23");
        acceptProgramClose("end program x", "x");
        acceptProgramClose("endprogram x", "x");
        acceptProgramClose("end\tprogram \t\txyz_123  \t", "xyz_123");
        acceptProgramClose("end\tprogram", null);
        acceptProgramClose("end\tprogram\t", null);
        for (int i = 0, n = "end".length(); i < n; ++i)
        {
            String str = flipChrCase("end", i) + " Program x";
            acceptProgramClose(str, "x");
        }
        for (int i = 0, n = "Program".length(); i < n; ++i)
        {
            String str = "end " + flipChrCase("Program", i) + " x";
            acceptProgramClose(str, "x");
        }
        rejectLine("\tend\t\tprogram x z");
    }

    void verifyBlockDataOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.BlockDataOpen, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.BlockDataOpen, line, name);
    }

    public void testBlockDataOpen() throws Exception
    {
        verifyBlockDataOpen("blockdata", null);
        verifyBlockDataOpen(" blockdata", null);
        verifyBlockDataOpen("blockdata ", null);
        verifyBlockDataOpen("block data", null);
        verifyBlockDataOpen("blockdata x", "x");
        verifyBlockDataOpen("block data x", "x");
        verifyBlockDataOpen("blockdata \t\txyz_123  \t", "xyz_123");
        verifyBlockDataOpen("block data \t\txyz_123  \t", "xyz_123");
    }

    void verifyBlockDataClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.BlockDataClose, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.BlockDataClose, line, name);
    }

    public void testBlockDataClose() throws Exception
    {
        verifyBlockDataClose("endblockdata", null);
        verifyBlockDataClose("endblockdata x", "x");
        verifyBlockDataClose("end blockdata x", "x");
        verifyBlockDataClose("end block data x", "x");
        verifyBlockDataClose("end\tblockdata \t\txyz_123  \t", "xyz_123");
        verifyBlockDataClose("end\tblockdata", null);
        verifyBlockDataClose("end\tblockdata\t", null);
    }

    void acceptProgramUnitClose(String line) throws FortranException, IOException, Exception
    {
        List<StatementInfo> statements = scanner.parse(Utils.toInputStream(line + "\n"));
        assertEquals(1, statements.size());
        StatementInfo info = statements.get(0);
        if (info instanceof CloseStatementInfo)
        {
            CloseStatementInfo refIUnfo = (CloseStatementInfo) info;
            assertEquals(null, refIUnfo.type);
            assertEquals(null, refIUnfo.name);
        }
    }

    public void testProgramUnitClose() throws Exception
    {
        acceptProgramUnitClose("end");
        acceptProgramUnitClose(" end");
        acceptProgramUnitClose("end ");
        acceptProgramUnitClose(" end ");
        for (int i = 0, n = "end".length(); i < n; ++i)
        {
            String str = flipChrCase("end", i);
            acceptProgramUnitClose(str);
        }
    }

    FortranDepStatementsRecognizer.UseModuleData acceptUseModule(String line, String refName)
            throws IOException, Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = parser.parseUse(line);
        assertEquals(refName, res.moduleName);
        verifyUseModuleStatementScan(line, refName);
        return res;
    }

    public void testUse() throws Exception
    {
        acceptUseModule("use x", "x");
        for (int i = 0, n = "use".length(); i < n; ++i)
        {
            String str = flipChrCase("use", i) + " x";
            acceptUseModule(str, "x");
        }
        acceptUseModule(" use x", "x");
        acceptUseModule(" use x ", "x");
        acceptUseModule("use x,y1=>z1", "x");
        acceptUseModule("use x,  y1=>z1", "x");
        acceptUseModule("use x,  y1  =>z1", "x");
        acceptUseModule("use x,  y1  =>  z1", "x");
        acceptUseModule("use x,  y1  =>  z1  ", "x");
        acceptUseModule("use x,  y1  =>  z1, y2 => z2 ", "x");
        acceptUseModule("use x,  y1  =>  z1, y2 => z2, y3 => z3 ", "x");
        acceptUseModule("use x,  ONLY: y1", "x");
        for (int i = 0, n = "only".length(); i < n; ++i)
        {
            String str = "use x,  " + flipChrCase("only", i) + ":x";
            acceptUseModule(str, "x");
        }
        acceptUseModule("use x ,  ONLY : y1", "x");
        acceptUseModule("use x,  ONLY : y1", "x");
        acceptUseModule("use x,  ONLY: y1,y2", "x");
        acceptUseModule("use x,  ONLY: y1,y2,y3", "x");
        acceptUseModule("use x,  ONLY:y1=>z1", "x");
        acceptUseModule("use x,  ONLY: y1 =>z1", "x");
        acceptUseModule("use x,  ONLY: y1  =>z1", "x");
        acceptUseModule("use x,  ONLY: y1  =>  z1", "x");
        acceptUseModule("use x,  ONLY: y1  =>  z1  ", "x");
        acceptUseModule("use x,  ONLY: y1  =>  z1, y2 => z2, y3 => z3 ", "x");
        rejectLine("usex");
        rejectLine("use x,");
        rejectLine("use x y ");
        rejectLine("use x, y,");
        rejectLine("use x, ONLY ");
        rejectLine("use x, ONLY: x y");
        FortranDepStatementsRecognizer.UseModuleData res = acceptUseModule("use x", "x");
        assertFalse(res.useOnly);
    }

    public void testUseWithRename() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = acceptUseModule("use x, to1 => from1, to2 => from2", "x");
        assertFalse(res.useOnly);
        assertEquals(2, res.useRenamedSymbols.size());
        assertEquals("from1", res.useRenamedSymbols.get(0).from);
        assertEquals("to1", res.useRenamedSymbols.get(0).to);
        assertEquals("from2", res.useRenamedSymbols.get(1).from);
        assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }

    public void testUseOnlyWithRename() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = acceptUseModule("use x, ONLY: to1 => from1, to2 => from2",
                "x");
        assertTrue(res.useOnly);
        assertEquals(2, res.useRenamedSymbols.size());
        assertEquals("from1", res.useRenamedSymbols.get(0).from);
        assertEquals("to1", res.useRenamedSymbols.get(0).to);
        assertEquals("from2", res.useRenamedSymbols.get(1).from);
        assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }

    public void testUseOnly() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = acceptUseModule("use x, ONLY: to1, to2", "x");
        assertTrue(res.useOnly);
        assertEquals(2, res.useSymbols.size());
        assertEquals("to1", res.useSymbols.get(0));
        assertEquals("to2", res.useSymbols.get(1));
    }

    String flipChrCase(String str, int idx)
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

    void acceptIdentifierString(String str) throws Exception
    {
        acceptModuleOpen("module " + str, str.toLowerCase());
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
        rejectLine("_");
        for (char l2 = '0'; l2 < '9'; ++l2)
        {
            rejectLine("" + l2);
        }
    }
}
