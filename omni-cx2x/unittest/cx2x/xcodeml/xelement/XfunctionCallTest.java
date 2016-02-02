/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test features of the XfunctionCall class.
 *
 * @author clementval
 */
public class XfunctionCallTest {

  private static final String basicFctCall =
      "<functionCall type=\"Fvoid\">" +
      "<name type=\"F7f84b9c0b200\">clawloop</name>" +
      "<arguments>" +
      "<Var type=\"A7f84b9c076b0\" scope=\"local\">value1</Var>" +
      "<Var type=\"A7f84b9c07f60\" scope=\"local\">value2</Var>" +
      "</arguments>" +
      "</functionCall>";

  @Test
  public void basicFunctionCallTest(){
    XfunctionCall fctCall =
        XmlHelper.createXfunctionCallFromString(basicFctCall);
    assertNotNull(fctCall);
    assertEquals(XelementName.TYPE_F_VOID, fctCall.getType());
    assertEquals("F7f84b9c0b200", fctCall.getName().getType());
    assertEquals("clawloop", fctCall.getName().getValue());
    assertEquals(2, fctCall.getArgumentsTable().count());

    assertTrue(fctCall.getArgumentsTable().hasArgument("value1"));
    assertNotNull(fctCall.getArgumentsTable().findArgument("value1"));
    assertTrue(fctCall.getArgumentsTable().hasArgument("value2"));
    assertNotNull(fctCall.getArgumentsTable().findArgument("value2"));

    XbaseElement arg1 = fctCall.getArgumentsTable().findArgument("value1");
    assertTrue(arg1 instanceof Xvar);
    Xvar var1 = (Xvar)arg1;
    assertEquals("A7f84b9c076b0", var1.getType());
    assertEquals(Xscope.LOCAL, var1.getScope());

    XbaseElement arg2 = fctCall.getArgumentsTable().findArgument("value2");
    assertTrue(arg2 instanceof Xvar);
    Xvar var2 = (Xvar)arg2;
    assertEquals("A7f84b9c07f60", var2.getType());
    assertEquals(Xscope.LOCAL, var2.getScope());

    assertNull(fctCall.getArgumentsTable().findArgument("foo"));
  }
}
