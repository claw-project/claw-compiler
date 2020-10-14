/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.depscan.FortranDepStatementsRecognizer;
import clawfc.depscan.FortranSyntaxException;

import junit.framework.TestCase;


public class FortranDepStatementsRecognizerTest 
    extends TestCase 
{
    private FortranDepStatementsRecognizer parser;
    
    @Override
    protected void setUp() throws Exception 
    {
    	parser = new FortranDepStatementsRecognizer();
    }
    
    public void testModuleOpen() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseModuleOpen("module x");
    	assertEquals("x", res.moduleOpenName);
    	res = parser.parseModuleOpen("\r\rmodule \t\txyz_123  \r\t");
    	assertEquals("xyz_123", res.moduleOpenName);
    }
    
    public void testModuleClose() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseModuleClose("end module x");
    	assertEquals("x", res.moduleCloseName);
    	res = parser.parseModuleClose("\rend\r\rmodule \t\txyz_123  \r\t");
    	assertEquals("xyz_123", res.moduleCloseName);
    }
    
    public void testProgramOpen() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseProgramOpen("program x");
    	assertEquals("x", res.programOpenName);
    	res = parser.parseProgramOpen("\r\rprogram \t\txyz_123  \r\t");
    	assertEquals("xyz_123", res.programOpenName);
    }
    
    public void testProgramClose() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseProgramClose("end program x");
    	assertEquals("x", res.programCloseName);
    	res = parser.parseProgramClose("\rend\r\rprogram \t\txyz_123  \r\t");
    	assertEquals("xyz_123", res.programCloseName);
    }
    
    public void testUse() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseUse("use x");
    	assertEquals("x", res.useModuleName);
    	assertFalse(res.useOnly);
    }
    
    public void testUseWithRename() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseUse("use x, to1 => from1, to2 => from2");
    	assertEquals("x", res.useModuleName);
    	assertFalse(res.useOnly);    	
    	assertEquals(2, res.useRenamedSymbols.size());
    	assertEquals("from1", res.useRenamedSymbols.get(0).from);
    	assertEquals("to1", res.useRenamedSymbols.get(0).to);
    	assertEquals("from2", res.useRenamedSymbols.get(1).from);
    	assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }
    
    public void testUseOnlyWithRename() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseUse("use x, ONLY: to1 => from1, to2 => from2");
    	assertEquals("x", res.useModuleName);
    	assertTrue(res.useOnly);    	
    	assertEquals(2, res.useRenamedSymbols.size());
    	assertEquals("from1", res.useRenamedSymbols.get(0).from);
    	assertEquals("to1", res.useRenamedSymbols.get(0).to);
    	assertEquals("from2", res.useRenamedSymbols.get(1).from);
    	assertEquals("to2", res.useRenamedSymbols.get(1).to);
    }
    
    public void testUseOnly() throws Exception
    {
    	FortranDepStatementsRecognizer.Data res = parser.parseUse("use x, ONLY: to1, to2");
    	assertEquals("x", res.useModuleName);
    	assertTrue(res.useOnly);    	
    	assertEquals(2, res.useSymbols.size());
    	assertEquals("to1", res.useSymbols.get(0));
    	assertEquals("to2", res.useSymbols.get(1));
    }
}
