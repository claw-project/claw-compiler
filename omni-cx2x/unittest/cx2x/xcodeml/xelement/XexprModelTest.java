/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import org.w3c.dom.Document;

import helper.XmlHelper;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.xelement.*;

/**
 * Test the features of the XexprModel class
 *
 * @author clementval
 */

public class XexprModelTest {

  private static final String dummyRootOpen = "<exprModel>";
  private static final String dummyRootClose = "</exprModel>";

  private static final String exprModel_IntConst = dummyRootOpen +
    "<FintConstant type=\"Fint\">16</FintConstant>" + dummyRootClose;

  private static final String exprModel_RealConst = dummyRootOpen +
    "<FrealConstant type=\"Freal\">9.81</FrealConstant>" + dummyRootClose;

  private static final String exprModel_ComplexConst = dummyRootOpen +
    "<FcomplexConstant type=\"Fcomplex\">" +
    "  <FrealConstant type=\"Freal\">1.0</FrealConstant>" +
    "  <FrealConstant type=\"Freal\">2.0</FrealConstant>" +
    "</FcomplexConstant>" + dummyRootClose;

  private static final String exprModel_LogConst = dummyRootOpen +
    "<FlogicalConstant type=\"Flogical\">.TRUE.</FlogicalConstant>" +
    dummyRootClose;

  private static final String exprModel_CharConst = dummyRootOpen +
    "<FcharacterConstant type=\"C7fca03c0d3c0\">value1: </FcharacterConstant>"
    + dummyRootClose;

  private static final String exprModel_Var = dummyRootOpen +
    "<Var type=\"Fint\" scope=\"local\">k</Var>" + dummyRootClose;

  private static final String exprModel_fctCall = dummyRootOpen +
    "<functionCall type=\"Fvoid\">" +
    "  <name type=\"F7fca03c08d80\">clawloop</name>" +
    "  <arguments>" +
    "    <Var type=\"A7fca03c07980\" scope=\"local\">value1</Var>" +
    "    <Var type=\"A7fca03c08230\" scope=\"local\">value2</Var>" +
    "  </arguments>" +
    "</functionCall>" + dummyRootClose;

  private static final String exprModel_arrayRef = dummyRootOpen +
      "<FarrayRef type=\"Rb0fd40\">" +
      "<varRef type=\"Ab0fe80\">" +
      "<Var type=\"Ab0fe80\" scope=\"local\">array1</Var>" +
      "</varRef>" +
      "<arrayIndex>" +
      "<Var type=\"Ib524e0\" scope=\"local\">i</Var>" +
      "</arrayIndex>" +
      "<arrayIndex>" +
      "<Var type=\"Ib52650\" scope=\"local\">j</Var>" +
      "</arrayIndex>" +
      "</FarrayRef>" +
      dummyRootClose;

  @Test
  public void findExprModelTest() {
    // XintConstant object
    Document xml = XmlHelper.loadXMLFromString(exprModel_IntConst);
    assertNotNull(xml);
    XbaseElement element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    XexprModel model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isIntConst());
    XintConstant iConst = model.getIntConstant();
    assertNotNull(iConst);
    assertEquals(XelementName.TYPE_F_INT, iConst.getType());
    assertEquals("16", iConst.getValue());

    // XrealConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_RealConst);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isRealConst());
    XrealConstant rConst = model.getRealConstant();
    assertNotNull(rConst);
    assertEquals(XelementName.TYPE_F_REAL, rConst.getType());
    assertEquals("9.81", rConst.getValue());

    // XcomplexConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_ComplexConst);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isComplexConst());

    // XlogicalConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_LogConst);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isLogicalConst());
    XlogicalConstant lConst = model.getLogicalConstant();
    assertNotNull(lConst);
    assertEquals(XelementName.TYPE_F_LOGICAL, lConst.getType());
    assertEquals(".TRUE.", lConst.getValue());

    // XcharacterConstant object
    xml = XmlHelper.loadXMLFromString(exprModel_CharConst);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isCharConst());
    XcharacterConstant cConst = model.getCharacterConstant();
    assertNotNull(cConst);
    assertEquals("C7fca03c0d3c0", cConst.getType());
    assertEquals("value1: ", cConst.getValue());

    // Xvar object
    xml = XmlHelper.loadXMLFromString(exprModel_Var);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isVar());
    Xvar var = model.getVar();
    assertNotNull(var);
    assertEquals(XelementName.TYPE_F_INT, var.getType());
    assertEquals(Xscope.LOCAL, var.getScope());
    assertEquals("k", var.getValue());

    // XfunctionCall
    xml = XmlHelper.loadXMLFromString(exprModel_fctCall);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertNotNull(model);
    assertTrue(model.isFctCall());
    XfunctionCall fCall = model.getFctCall();
    assertNotNull(fCall);
    assertEquals(XelementName.TYPE_F_VOID, fCall.getType());
    assertEquals("clawloop", fCall.getName().getValue());
    assertEquals("F7fca03c08d80", fCall.getName().getType());
    assertEquals(2, fCall.getArgumentsTable().count());

    // ArrayRef
    xml = XmlHelper.loadXMLFromString(exprModel_arrayRef);
    assertNotNull(xml);
    element = new XbaseElement(xml.getDocumentElement());
    assertNotNull(element);
    model = XelementHelper.findExprModel(element, false);
    assertTrue(model.isArrayRef());
    assertNotNull(model.getArrayRef());
  }

}
