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

  private String exprModel_LogConst = dummyRootOpen +
    "<FlogicalConstant>0</FlogicalConstant>" + dummyRootClose; // TODO check with real XcodeML

  private String exprModel_CharConst = dummyRootOpen +
    "<FcharacterConstant type=\"C7fca03c0d3c0\">value1: </FcharacterConstant>"
    + dummyRootClose;

  private String exprModel_Var = dummyRootOpen +
    "<Var type=\"Fint\" scope=\"local\">k</Var>" + dummyRootClose;

  @Test
  public void findExprModelTest() {
    // XintConstant object
    Document xml = XmlHelper.loadXMLFromString(exprModel_IntConst);
    XbaseElement element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    XexprModel model = XelementHelper.findExprModel(element);
    assertNotNull(model);
    assertTrue(model.isIntConst());

    // XrealConstant object TODO
    // XcomplexConstant object TODO

    // XlogicalConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_LogConst);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element);
    assertNotNull(model);
    assertTrue(model.isLogicalConst());

    // XcharacterConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_CharConst);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element);
    assertNotNull(model);
    assertTrue(model.isCharConst());

    // Xvar object
    xml = XmlHelper.loadXMLFromString(exprModel_Var);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element);
    assertNotNull(model);
    assertTrue(model.isVar());
  }

}
