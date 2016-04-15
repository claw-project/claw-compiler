/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import helper.XmlHelper;
import org.junit.Test;

/**
 * Test the featues of the XintConstant, XrealConstant, XlogicalConstant,
 * XcharachterConstant classes.
 *
 * @author clementval
 */
public class XconstantTest {


  private static final String intConst1 =
      "<FintConstant type=\"Fint\">10</FintConstant>";

  private static final String realConst1 =
      "<FrealConstant type=\"Freal\" kind=\"dp\">80.0</FrealConstant>";

  private static final String logConst1 =
      "<FlogicalConstant type=\"Flogical\">.FALSE.</FlogicalConstant>";

  private static final String charConst1 =
      "<FcharacterConstant type=\"Fcharacter\"> </FcharacterConstant>";

  private static final String charConst2 =
      "<FcharacterConstant type=\"Ce47ed0\">****</FcharacterConstant>";

  private static final String cmplxConst1 =
      "<FcomplexConstant type=\"Fcomplex\">" +
      "<FrealConstant type=\"Freal\">1.0</FrealConstant>" +
      "<FrealConstant type=\"Freal\">2.0</FrealConstant>" +
      "</FcomplexConstant>";


  @Test
  public void intConstantTest(){
    XintConstant intConst = XmlHelper.createIntConstantFromString(intConst1);
    assertNotNull(intConst);
    assertTrue(intConst.hasType());
    assertEquals(XelementName.TYPE_F_INT, intConst.getType());
    assertEquals("10", intConst.getValue());
    assertFalse(intConst.hasKind());
    assertNull(intConst.getKind());

    intConst.setKind("wp");
    assertEquals("wp", intConst.getKind());
    intConst.setValue("9");
    assertEquals("9", intConst.getValue());
  }

  @Test
  public void realConstantTest(){
    XrealConstant realConst =
        XmlHelper.createRealConstantFromString(realConst1);
    assertNotNull(realConst);
    assertTrue(realConst.hasType());
    assertEquals(XelementName.TYPE_F_REAL, realConst.getType());
    assertEquals("80.0", realConst.getValue());
    assertTrue(realConst.hasKind());
    assertEquals("dp", realConst.getKind());
  }

  @Test
  public void logicalConstantTest(){
    XlogicalConstant logConst =
        XmlHelper.createLogicalConstantFromString(logConst1);
    assertNotNull(logConst);
    assertFalse(logConst.hasKind());
    assertNull(logConst.getKind());
    assertTrue(logConst.hasType());
    assertEquals(XelementName.TYPE_F_LOGICAL, logConst.getType());
    assertEquals(".FALSE.", logConst.getValue());
  }

  @Test
  public void characterConstantTest(){
    XcharacterConstant charConst =
        XmlHelper.createCharConstantFromString(charConst1);
    assertNotNull(charConst);
    assertFalse(charConst.hasKind());
    assertNull(charConst.getKind());
    assertTrue(charConst.hasType());
    assertEquals(XelementName.TYPE_F_CHAR, charConst.getType());
    assertEquals(" ", charConst.getValue());
  }

  @Test
  public void characterConstantWithSpecialTypeTest(){
    XcharacterConstant charConst =
        XmlHelper.createCharConstantFromString(charConst2);
    assertNotNull(charConst);
    assertFalse(charConst.hasKind());
    assertNull(charConst.getKind());
    assertTrue(charConst.hasType());
    assertEquals("Ce47ed0", charConst.getType());
    assertEquals("****", charConst.getValue());
  }

  @Test
  public void complexConstantTest(){
    XcomplexConstant cmplx = XmlHelper.createXcomplexConstant(cmplxConst1);
    assertNotNull(cmplx);
    assertEquals(XelementName.TYPE_F_COMPLEX, cmplx.getType());
    assertNotNull(cmplx.getRealPart());
    assertNotNull(cmplx.getImaginaryPart());
    assertEquals(XelementName.TYPE_F_REAL, cmplx.getRealPart().getType());
    assertEquals("1.0", cmplx.getRealPart().getValue());
    assertEquals(XelementName.TYPE_F_REAL, cmplx.getImaginaryPart().getType());
    assertEquals("2.0", cmplx.getImaginaryPart().getValue());
  }


  @Test
  public void fromScratchTest(){
    XcodeProgram program = XmlHelper.getDummyXcodeProgram();
    XintConstant intConst = null;
    try {
      intConst = XelementHelper.createEmpty(XintConstant.class, program);
    } catch (IllegalTransformationException itex){
      fail();
    }

    intConst.setType(XelementName.TYPE_F_INT);
    intConst.setValue("10");
    assertNotNull(intConst);
    assertTrue(intConst.hasType());
    assertEquals(XelementName.TYPE_F_INT, intConst.getType());
    assertEquals("10", intConst.getValue());
    assertFalse(intConst.hasKind());
    assertNull(intConst.getKind());
  }
}
