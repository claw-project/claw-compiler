/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test features of basic elements
 *
 * @author clementval
 */
public class XbasicTest {

  private static final String NAME1 = "<name type=\"Fint\">a</name>";
  private static final String VALUE1 = "<value>" +
      "<FintConstant type=\"Fint\">1</FintConstant></value>";

  @Test
  public void xValueTest() {
    Xnode val = XmlHelper.createXnode(VALUE1);
    assertNotNull(val);
    assertSame(val.child(0).opcode(), Xcode.F_INT_CONSTANT);
    assertEquals(Xname.TYPE_F_INT, val.child(0).getType());
    assertEquals("1", val.child(0).value());
  }

  @Test
  public void xNameTest() {
    Xnode name = XmlHelper.createXnode(NAME1);
    assertNotNull(name);
    assertEquals(Xname.TYPE_F_INT, name.getType());
    assertEquals("a", name.value());
  }
}
