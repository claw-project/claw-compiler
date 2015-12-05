package xelement;

import static org.junit.Assert.*;
import org.junit.Test;

import org.w3c.dom.Element;

import helper.XmlHelper;

import x2x.translator.xcodeml.xelement.Xvar;
import x2x.translator.xcodeml.xelement.Xscope;
import x2x.translator.xcodeml.xelement.XarrayRef;

public class XarrayRefTest {
  private static final String ARRAYREF_TYPE = "Fint";
  private static final String TEST_TYPE = "F7f81a04070d0";
  private static final Xscope TEST_SCOPE = Xscope.GLOBAL;
  private static final String TEST_VALUE = "j";

  private Xvar createXvar(){
    String simpleVarElement = "<Var type=\"" + TEST_TYPE + "\" scope=\"" +
      TEST_SCOPE + "\">" + TEST_VALUE + "</Var>";
    return XmlHelper.createXvarFromString(simpleVarElement);
  }

  @Test
  public void transformVarToArrayRef() {
    File f = new File(TEST_DATA);
    assertTrue(f.exists());
    XcodeProg xcodeml = new XcodeProg(TEST_DATA);
    xcodeml.load();
    xcodeml.readTypeTable();
    xcodeml.readGlobalSymbolsTable();

    

    XarrayRef arrayRef = XarrayRef.create(xcodeml, var, ARRAYREF_TYPE, XarrayIndex index);



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
