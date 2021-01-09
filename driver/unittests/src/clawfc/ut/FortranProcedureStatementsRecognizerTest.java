/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.ut;

import java.io.IOException;
import java.util.List;

import clawfc.Utils;
import clawfc.depscan.FortranDepParser;
import clawfc.depscan.FortranDepParser.CloseStatementInfo;
import clawfc.depscan.FortranDepParser.OpenStatementInfo;
import clawfc.depscan.FortranDepParser.StatementInfo;
import clawfc.depscan.FortranException;
import clawfc.depscan.FortranProcedureStatementsRecognizer;
import clawfc.depscan.FortranProgramUnitStatementsRecognizer.StatementType;
import junit.framework.TestCase;

public class FortranProcedureStatementsRecognizerTest extends TestCase
{
    private FortranProcedureStatementsRecognizer parser;
    private FortranDepParser scanner;

    @Override
    protected void setUp() throws Exception
    {
        parser = new FortranProcedureStatementsRecognizer();
        scanner = new FortranDepParser();
    }

    public void verifyProgramUnitStatementScan(final StatementType refType, final String line, final String refName)
            throws FortranException, IOException, Exception
    {
        List<StatementInfo> statements = scanner.parse(Utils.toInputStream(line + ";"));
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

    void acceptSubroutineOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.SubroutineOpen, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.SubroutineOpen, line, name);
    }

    public void testSubroutineOpen() throws Exception
    {
        acceptSubroutineOpen(" subroutine f", "f");
        acceptSubroutineOpen("subroutine f", "f");
        acceptSubroutineOpen("subroutine f()", "f");
        acceptSubroutineOpen("subroutine f ()", "f");
        acceptSubroutineOpen("subroutine f(a)", "f");
        acceptSubroutineOpen("subroutine f(a, b)", "f");
        acceptSubroutineOpen("subroutine f(a, b)", "f");
        acceptSubroutineOpen("pure subroutine f(a, b)", "f");
        acceptSubroutineOpen("pure elemental subroutine f(a, b)", "f");
        acceptSubroutineOpen("pure elemental recursive subroutine f(a, b)", "f");
        acceptSubroutineOpen(" subroutine f bind(C)", "f");
        acceptSubroutineOpen(" subroutine f bind ( C ) ", "f");
        acceptSubroutineOpen(" subroutine f bind(C,name=\"f\")", "f");
        acceptSubroutineOpen(" subroutine f bind(C,name='f')", "f");
        acceptSubroutineOpen(" subroutine f bind ( C , name = 'f' ) ", "f");
    }

    void acceptSubroutineClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.SubroutineClose, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.SubroutineClose, line, name);
    }

    public void testSubroutineClose() throws Exception
    {
        acceptSubroutineClose("end subroutine x", "x");
        acceptSubroutineClose("endsubroutine x", "x");
        acceptSubroutineClose("end\tsubroutine \t\txyz_123  \t", "xyz_123");
        acceptSubroutineClose("end\tsubroutine", null);
        acceptSubroutineClose("end\tsubroutine\t", null);
    }

    void acceptFunctionOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.FunctionOpen, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.FunctionOpen, line, name);
    }

    public void testFunctionOpen() throws Exception
    {
        acceptFunctionOpen("function f()", "f");
        acceptFunctionOpen("function f ()", "f");
        acceptFunctionOpen("function f ( )", "f");
        acceptFunctionOpen("function function()", "function");
        acceptFunctionOpen("function f(a)", "f");
        acceptFunctionOpen("function f(a, b)", "f");
        acceptFunctionOpen("function f(a, b)", "f");
        acceptFunctionOpen("pure function f(a, b)", "f");
        acceptFunctionOpen("pure elemental function f(a, b)", "f");
        acceptFunctionOpen("pure elemental recursive function f(a, b)", "f");
        acceptFunctionOpen("function f(a, b)result(r)", "f");
        acceptFunctionOpen("function f(a, b) result(r)", "f");
        acceptFunctionOpen("type(t1) function f(a, b) result(r)", "f");
        acceptFunctionOpen("type(t1)function f(a, b) result(r)", "f");
        acceptFunctionOpen("logical function f(a, b) result(r)", "f");
        acceptFunctionOpen("character function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer function f(a, b) result(r)", "f");
        acceptFunctionOpen("real function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer function f(a, b) result(r)", "f");
        acceptFunctionOpen("doubleprecision function f(a, b)", "f");
        acceptFunctionOpen("double precision function f(a, b)", "f");
        acceptFunctionOpen("integer(kind=1) function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer(kind=1+2) function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer(kind=(1+2)) function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer(kind=(1+(1 + 1))) function f(a, b) result(r)", "f");
        acceptFunctionOpen("integer(kind=(1+2), len=(3+4)) function f(a, b) result(r)", "f");
        acceptFunctionOpen(" pure integer(kind=(1+2), len=(3+4)) function f(a, b) result(r)", "f");
        acceptFunctionOpen(" function f() bind(C)", "f");
        acceptFunctionOpen(" function f() bind ( C ) ", "f");
        acceptFunctionOpen(" function f() bind(C,name=\"f\")", "f");
        acceptFunctionOpen(" function f() bind(C,name='f')", "f");
        acceptFunctionOpen(" function f() bind ( C , name = 'f' ) ", "f");
        acceptFunctionOpen(" function f() bind(C) result(x)", "f");
        acceptFunctionOpen(" function f() result(x) bind(C)", "f");
    }

    void verifyFunctionClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseUnitStatement(StatementType.FunctionClose, line);
        assertEquals(name, unitName);
        verifyProgramUnitStatementScan(StatementType.FunctionClose, line, name);
    }

    public void testFunctionClose() throws Exception
    {
        verifyFunctionClose("end function x", "x");
        verifyFunctionClose("endfunction x", "x");
        verifyFunctionClose("end\tfunction \t\txyz_123  \t", "xyz_123");
        verifyFunctionClose("end\tfunction", null);
        verifyFunctionClose("end\tfunction\t", null);
    }

}
