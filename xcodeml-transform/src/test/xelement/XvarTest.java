package xelement;

import static org.junit.Assert.*;
import org.junit.Test;

import org.w3c.dom.Element;

import helper.XmlHelper;

import x2x.translator.xcodeml.xelement.Xvar;

public class XvarTest {
  private static final String TEST_TYPE = "Fint";
  private static final String TEST_SCOPE = "local";
  private static final String TEST_VALUE = "i";

  private static final String ALT_TEST_TYPE = "F7f81a04070d0";
  private static final String ALT_TEST_SCOPE = "global";
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
  }

}
