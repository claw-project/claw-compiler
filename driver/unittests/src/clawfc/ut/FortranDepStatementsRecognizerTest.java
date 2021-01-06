/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.io.IOException;

import clawfc.depscan.FortranDepStatementsRecognizer;
import junit.framework.TestCase;

public class FortranDepStatementsRecognizerTest extends TestCase
{
    private FortranDepStatementsRecognizer parser;

    @Override
    protected void setUp() throws Exception
    {
        parser = new FortranDepStatementsRecognizer();
    }

    public void testModuleOpen() throws Exception
    {
        String moduleOpenName = parser.parseModuleOpen("module x");
        assertEquals("x", moduleOpenName);
        moduleOpenName = parser.parseModuleOpen("module \t\txyz_123  \t");
        assertEquals("xyz_123", moduleOpenName);
    }

    void verifyModuleClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseModuleClose(line);
        assertEquals(name, unitName);
    }

    public void testModuleClose() throws Exception
    {
        verifyModuleClose("end", null);
        verifyModuleClose(" end", null);
        verifyModuleClose("end ", null);
        verifyModuleClose(" end ", null);
        verifyModuleClose("endmodule x", "x");
        verifyModuleClose("end module x", "x");
        verifyModuleClose("end\tmodule \t\txyz_123  \t", "xyz_123");
        verifyModuleClose("end\tmodule", null);
        verifyModuleClose("end\tmodule\t", null);
    }

    public void testProgramOpen() throws Exception
    {
        String programOpenName = parser.parseProgramOpen("program x");
        assertEquals("x", programOpenName);
        programOpenName = parser.parseProgramOpen("program \t\txyz_123  \t");
        assertEquals("xyz_123", programOpenName);
    }

    void verifyProgramClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseProgramClose(line);
        assertEquals(name, unitName);
    }

    public void testProgramClose() throws Exception
    {
        verifyProgramClose("end", null);
        verifyProgramClose(" end", null);
        verifyProgramClose("end ", null);
        verifyProgramClose(" end ", null);
        verifyProgramClose("end program x", "x");
        verifyProgramClose("endprogram x", "x");
        verifyProgramClose("end\tprogram \t\txyz_123  \t", "xyz_123");
        verifyProgramClose("end\tprogram", null);
        verifyProgramClose("end\tprogram\t", null);
    }

    void verifyBlockDataOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseBlockDataOpen(line);
        assertEquals(name, unitName);
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
        final String unitName = parser.parseBlockDataClose(line);
        assertEquals(name, unitName);
    }

    public void testBlockDataClose() throws Exception
    {
        verifyBlockDataClose("end", null);
        verifyBlockDataClose(" end", null);
        verifyBlockDataClose("end ", null);
        verifyBlockDataClose(" end ", null);
        verifyBlockDataClose("endblockdata", null);
        verifyBlockDataClose("endblockdata x", "x");
        verifyBlockDataClose("end blockdata x", "x");
        verifyBlockDataClose("end block data x", "x");
        verifyBlockDataClose("end\tblockdata \t\txyz_123  \t", "xyz_123");
        verifyBlockDataClose("end\tblockdata", null);
        verifyBlockDataClose("end\tblockdata\t", null);
    }

    public void testUse() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = parser.parseUse("use x");
        assertEquals("x", res.moduleName);
        assertFalse(res.useOnly);
    }

    public void testUseWithRename() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = parser.parseUse("use x, to1 => from1, to2 => from2");
        assertEquals("x", res.moduleName);
        assertFalse(res.useOnly);
        assertEquals(2, res.useRenamedSymbols.size());
        assertEquals("from1", res.useRenamedSymbols.get(0).from);
        assertEquals("to1", res.useRenamedSymbols.get(0).to);
        assertEquals("from2", res.useRenamedSymbols.get(1).from);
        assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }

    public void testUseOnlyWithRename() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = parser.parseUse("use x, ONLY: to1 => from1, to2 => from2");
        assertEquals("x", res.moduleName);
        assertTrue(res.useOnly);
        assertEquals(2, res.useRenamedSymbols.size());
        assertEquals("from1", res.useRenamedSymbols.get(0).from);
        assertEquals("to1", res.useRenamedSymbols.get(0).to);
        assertEquals("from2", res.useRenamedSymbols.get(1).from);
        assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }

    public void testUseOnly() throws Exception
    {
        FortranDepStatementsRecognizer.UseModuleData res = parser.parseUse("use x, ONLY: to1, to2");
        assertEquals("x", res.moduleName);
        assertTrue(res.useOnly);
        assertEquals(2, res.useSymbols.size());
        assertEquals("to1", res.useSymbols.get(0));
        assertEquals("to2", res.useSymbols.get(1));
    }
}
