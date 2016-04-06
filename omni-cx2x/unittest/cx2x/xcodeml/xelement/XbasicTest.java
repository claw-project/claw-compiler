/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test features of basic elements
 *
 * @author clementval
 */
public class XbasicTest {

  private static final String name1 = "<name type=\"Fint\">a</name>";

  private static final String value1 =
      "<value>" +
      "<FintConstant type=\"Fint\">1</FintConstant>\n" +
      "</value>";


  @Test
  public void xValueTest(){
    Xvalue val = XmlHelper.createXvalue(value1);
    assertNotNull(val);
    assertTrue(val.getExprModel().isIntConst());
    assertEquals(XelementName.TYPE_F_INT,
        val.getExprModel().getIntConstant().getType());
    assertEquals("1", val.getExprModel().getIntConstant().getValue());
  }

  @Test
  public void xNameTest(){
    Xname name = XmlHelper.createXname(name1);
    assertNotNull(name);
    assertEquals(XelementName.TYPE_F_INT, name.getType());
    assertEquals("a", name.getValue());
  }

}
