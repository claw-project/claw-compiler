package xelement;

import static org.junit.Assert.*;
import org.junit.Test;

import org.w3c.dom.Element;

import helper.XmlHelper;

import x2x.translator.xcodeml.xelement.Xid;

public class XidTest {
  private static final String TEST_TYPE = "F7f9502e03d00";
  private static final String TEST_SCLASS = "ffunc";
  private static final String TEST_NAME = "loop_extract";

  private static final String ALT_TEST_TYPE = "F7f81a04070d0";
  private static final String ALT_TEST_SCLASS = "auto";
  private static final String ALT_TEST_NAME = "func_name";

  private Xid createXidFromString(){
    String simpleIdElement = "<id type=\"" + TEST_TYPE + "\" sclass=\"" +
      TEST_SCLASS + "\"><name>" + TEST_NAME + "</name></id>";
    Element id = XmlHelper.getElementFromString(simpleIdElement);
    assertNotNull(id);
    return new Xid(id);
  }

  @Test
  public void readElementInformationTest() {
    Xid simpleId = createXidFromString();
    assertEquals(TEST_NAME, simpleId.getName());
    assertEquals(TEST_TYPE, simpleId.getType());
    assertEquals(TEST_SCLASS, simpleId.getSclass());
  }

  @Test
  public void setElementInformationTest() {
    Xid simpleId = createXidFromString();
    simpleId.setName(ALT_TEST_NAME);
    simpleId.setType(ALT_TEST_TYPE);
    simpleId.setSclass(ALT_TEST_SCLASS);

    assertEquals(ALT_TEST_NAME, simpleId.getName());
    assertEquals(ALT_TEST_TYPE, simpleId.getType());
    assertEquals(ALT_TEST_SCLASS, simpleId.getSclass());
  }


}
