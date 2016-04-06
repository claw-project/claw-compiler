/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import helper.XmlHelper;

/**
 * Test the features of the Xvar class
 *
 * @author clementval
 */

public class XvarTest {
  private static final String TEST_TYPE = "Fint";
  private static final Xscope TEST_SCOPE = Xscope.LOCAL;
  private static final String TEST_VALUE = "i";

  private static final String ALT_TEST_TYPE = "F7f81a04070d0";
  private static final Xscope ALT_TEST_SCOPE = Xscope.GLOBAL;
  private static final String ALT_TEST_VALUE = "j";

  private Xvar createSimpleXvar(){
    String simpleVarElement = "<Var type=\"" + TEST_TYPE + "\" scope=\"" +
      TEST_SCOPE + "\">" + TEST_VALUE + "</Var>";
    return XmlHelper.createXvarFromString(simpleVarElement);
  }

  @Test
  public void readElementInformationTest() {
    Xvar element = createSimpleXvar();
    assertEquals(TEST_VALUE, element.getValue());
    assertEquals(TEST_TYPE, element.getType());
    assertEquals(TEST_SCOPE, element.getScope());
  }

  @Test
  public void setElementInformationTest() {
    Xvar element = createSimpleXvar();
    element.setValue(ALT_TEST_VALUE);
    element.setType(ALT_TEST_TYPE);
    element.setScope(ALT_TEST_SCOPE);

    assertEquals(ALT_TEST_VALUE, element.getValue());
    assertEquals(ALT_TEST_TYPE, element.getType());
    assertEquals(ALT_TEST_SCOPE, element.getScope());

    Xvar clone = element.cloneObject();
    assertEquals(ALT_TEST_VALUE, clone.getValue());
    assertEquals(ALT_TEST_TYPE, clone.getType());
    assertEquals(ALT_TEST_SCOPE, clone.getScope());
  }

}
