/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test Xsymbols and XglobalSymbols features
 *
 * @author clementval
 */
public class XsymbolsTest
{

    private static final String gSym1 = "<globalSymbols>" + "<id sclass=\"ffunc\">" + "<name>radiation_rg</name>"
            + "</id>" + "</globalSymbols>";

    private static final String sym2 = "<symbols>" + "<id type=\"I7fcbf34041b0\" sclass=\"flocal\" "
            + "declared_in=\"mo_kind\">" + "<name>dp</name>" + "</id>" + "<id type=\"I7fcbf3409ec0\" sclass=\"flocal\" "
            + "declared_in=\"mo_kind\">" + "<name>sp</name>" + "</id>" + "</symbols>";

    @Test
    public void addToTableTest()
    {
        Context context = new TestContext();
        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);
        assertNotNull(xcodeml);
        XsymbolTable table = xcodeml.getGlobalSymbolsTable();
        assertNotNull(table);

        Xid id1 = xcodeml.createId(FortranType.INTEGER, XstorageClass.F_LOCAL, "id1");
        table.add(id1);
        assertTrue(table.contains("id1"));
    }

    @Test
    public void simpleGlobalSymbolsTest()
    {
        XsymbolTable table = XmlHelper.createXSymbolTableFromString(gSym1);
        assertNotNull(table);
        assertEquals(1, table.size());

        Xid id1 = table.get("radiation_rg");
        assertNotNull(id1);
        assertEquals("ffunc", id1.getSclass());
        assertNull(id1.getType());

        assertFalse(table.contains("dummy"));
        assertNull(table.get("dummy"));
    }

    @Test
    public void simpleSymbolsTest()
    {
        XsymbolTable table = XmlHelper.createXSymbolTableFromString(sym2);
        assertTable(table);

        XsymbolTable clone = table.cloneNode();
        assertNotEquals(clone.element(), table.element());
        assertTable(clone);
    }

    private void assertTable(XsymbolTable table)
    {
        assertNotNull(table);
        assertEquals(2, table.size());

        Xid id1 = table.get("dp");
        assertNotNull(id1);
        assertEquals("flocal", id1.getSclass());
        assertEquals("I7fcbf34041b0", id1.getType());

        Xid id2 = table.get("sp");
        assertNotNull(id2);
        assertEquals("flocal", id2.getSclass());
        assertEquals("I7fcbf3409ec0", id2.getType());
    }
}
