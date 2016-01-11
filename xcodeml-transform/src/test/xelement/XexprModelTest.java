package xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import org.w3c.dom.Element;
import org.w3c.dom.Document;

import helper.XmlHelper;
import x2x.translator.xcodeml.xelement.*;

public class XexprModelTest {

  private String dummyRootOpen = "<exprModel>";
  private String dummyRootClose = "</exprModel>";

  private String exprModel_IntConst = dummyRootOpen +
    "<FintConstant type=\"Fint\">16</FintConstant>" + dummyRootClose;



  @Test
  public void findExprModelTest() {
    Document xml = XmlHelper.loadXMLFromString(exprModel_IntConst);
    XbaseElement element = new XbaseElement(xml.getDocumentElement());
    System.out.println(exprModel_IntConst);
    assertNotNull(element);
    XexprModel model = XelementHelper.findExprModel(element);
    assertNotNull(model);
    assertTrue(model.isIntConst());
  }

}
