/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.xelement.*;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test Xsymbols and XglobalSymbols features
 *
 * @author clementval
 */
public class XsymbolsTest {

  private static final String gSym1 =
      "<globalSymbols>" +
      "<id sclass=\"ffunc\">" +
      "<name>radiation_rg</name>" +
      "</id>" +
      "</globalSymbols>";

  private static final String sym2 =
      "<symbols>" +
      "<id type=\"I7fcbf34041b0\" sclass=\"flocal\" declared_in=\"mo_kind\">" +
      "<name>dp</name>" +
      "</id>" +
      "<id type=\"I7fcbf3409ec0\" sclass=\"flocal\" declared_in=\"mo_kind\">" +
      "<name>sp</name>" +
      "</id>" +
      "</symbols>";

  @Test
  public void simpleGlobalSymbolsTest(){
    XglobalSymbolTable table = XmlHelper.createXglobalSymbolFromString(gSym1);
    assertNotNull(table);
    assertEquals(1, table.count());

    Xid id1 = table.get("radiation_rg");
    assertNotNull(id1);
    assertEquals("ffunc", id1.getSclass());
    assertNull(id1.getType());
  }

  @Test
  public void simpleSymbolsTest(){
    XsymbolTable table = XmlHelper.createXSymbolTableFromString(sym2);
    assertNotNull(table);
    assertEquals(2, table.count());

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
