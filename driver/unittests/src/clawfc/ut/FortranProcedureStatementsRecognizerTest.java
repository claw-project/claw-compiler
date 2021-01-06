/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2021, MeteoSwiss
 */
package clawfc.ut;

import java.io.IOException;

import clawfc.depscan.FortranProcedureStatementsRecognizer;
import junit.framework.TestCase;

public class FortranProcedureStatementsRecognizerTest extends TestCase
{
    private FortranProcedureStatementsRecognizer parser;

    @Override
    protected void setUp() throws Exception
    {
        parser = new FortranProcedureStatementsRecognizer();
    }

    void verifySubroutineOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseSubroutineOpen(line);
        assertEquals(name, unitName);
    }

    public void testSubroutineOpen() throws Exception
    {
        verifySubroutineOpen(" subroutine f", "f");
        verifySubroutineOpen("subroutine f", "f");
        verifySubroutineOpen("subroutine f()", "f");
        verifySubroutineOpen("subroutine f ()", "f");
        verifySubroutineOpen("subroutine f(a)", "f");
        verifySubroutineOpen("subroutine f(a, b)", "f");
        verifySubroutineOpen("subroutine f(a, b)", "f");
        verifySubroutineOpen("pure subroutine f(a, b)", "f");
        verifySubroutineOpen("pure elemental subroutine f(a, b)", "f");
        verifySubroutineOpen("pure elemental recursive subroutine f(a, b)", "f");
    }

    void verifySubroutineClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseSubroutineClose(line);
        assertEquals(name, unitName);
    }

    public void testSubroutineClose() throws Exception
    {
        verifySubroutineClose("end", null);
        verifySubroutineClose(" end", null);
        verifySubroutineClose("end ", null);
        verifySubroutineClose(" end ", null);
        verifySubroutineClose("end subroutine x", "x");
        verifySubroutineClose("endsubroutine x", "x");
        verifySubroutineClose("end\tsubroutine \t\txyz_123  \t", "xyz_123");
        verifySubroutineClose("end\tsubroutine", null);
        verifySubroutineClose("end\tsubroutine\t", null);
    }

    void verifyFunctionOpen(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseFunctionOpen(line);
        assertEquals(name, unitName);
    }

    public void testFunctionOpen() throws Exception
    {
        verifyFunctionOpen("function f()", "f");
        verifyFunctionOpen("function f ()", "f");
        verifyFunctionOpen("function f ( )", "f");
        verifyFunctionOpen("function function()", "function");
        verifyFunctionOpen("function f(a)", "f");
        verifyFunctionOpen("function f(a, b)", "f");
        verifyFunctionOpen("function f(a, b)", "f");
        verifyFunctionOpen("pure function f(a, b)", "f");
        verifyFunctionOpen("pure elemental function f(a, b)", "f");
        verifyFunctionOpen("pure elemental recursive function f(a, b)", "f");
        verifyFunctionOpen("function f(a, b)result(r)", "f");
        verifyFunctionOpen("function f(a, b) result(r)", "f");
        verifyFunctionOpen("type(t1) function f(a, b) result(r)", "f");
        verifyFunctionOpen("type(t1)function f(a, b) result(r)", "f");
        verifyFunctionOpen("logical function f(a, b) result(r)", "f");
        verifyFunctionOpen("character function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer function f(a, b) result(r)", "f");
        verifyFunctionOpen("real function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer function f(a, b) result(r)", "f");
        verifyFunctionOpen("doubleprecision function f(a, b)", "f");
        verifyFunctionOpen("double precision function f(a, b)", "f");
        verifyFunctionOpen("integer(kind=1) function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer(kind=1+2) function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer(kind=(1+2)) function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer(kind=(1+(1 + 1))) function f(a, b) result(r)", "f");
        verifyFunctionOpen("integer(kind=(1+2), len=(3+4)) function f(a, b) result(r)", "f");
        verifyFunctionOpen(" pure integer(kind=(1+2), len=(3+4)) function f(a, b) result(r)", "f");
    }

    void verifyFunctionClose(String line, String name) throws IOException, Exception
    {
        final String unitName = parser.parseFunctionClose(line);
        assertEquals(name, unitName);
    }

    public void testFunctionClose() throws Exception
    {
        verifyFunctionClose("end", null);
        verifyFunctionClose(" end", null);
        verifyFunctionClose("end ", null);
        verifyFunctionClose(" end ", null);
        verifyFunctionClose("end function x", "x");
        verifyFunctionClose("endfunction x", "x");
        verifyFunctionClose("end\tfunction \t\txyz_123  \t", "xyz_123");
        verifyFunctionClose("end\tfunction", null);
        verifyFunctionClose("end\tfunction\t", null);
    }

}
