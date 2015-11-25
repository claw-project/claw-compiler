package xelement;

import static org.junit.Assert.*;
import org.junit.Test;

import org.w3c.dom.Element;

import helper.XmlHelper;

import x2x.translator.xcodeml.xelement.Xid;

public class XidTest {


  private Xid createXidFromString(){
    String simpleIdElement = "<id type=\"F7f9502e03d00\" sclass=\"ffunc\"><name>loop_extract</name></id>";
    Element id = XmlHelper.getElementFromString(simpleIdElement);
    assertNotNull(id);
    return new Xid(id);
  }


  @Test
  public void readElementInformationTest() {
    Xid simpleId = createXidFromString();
    assertEquals("loop_extract", simpleId.getName());
    assertEquals("F7f9502e03d00", simpleId.getType());
    assertEquals("ffunc", simpleId.getSclass());
  }

  @Test
  public void setElementInformationTest() {
    Xid simpleId = createXidFromString();
    simpleId.setName("new_name");
    simpleId.setType("new_type");
    simpleId.setSclass("auto");

    assertEquals("new_name", simpleId.getName());
    assertEquals("new_type", simpleId.getType());
    assertEquals("auto", simpleId.getSclass());
  }


}
