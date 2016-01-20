/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package xelement;

import cx2x.xcodeml.xelement.*;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * @author clementval
 */
public class XbasicTypeTest {

  private static final String type1 =
      "<FbasicType type=\"TYPE_NAME\" ref=\"Fint\">\n" +
      "<kind>8</kind>"+
      "</FbasicType>";

  private static final String type2 =
      "<FbasicType type=\"TYPE_NAME\" ref=\"Fcharacter\"> <len>\n" +
      "<FintConstant type=\"Fint\">10</FintConstant> </len>\n" +
      "</FbasicType>";

  private static final String type3 =
      "<FbasicType type=\"TYPE_NAME\" ref=\"Fint\">" +
      "<arrayIndex>" +
      "<FintConstant type=\"Fint\">10</FintConstant>" +
      "</arrayIndex>" +
      "<indexRange>" +
      "<lowerBound>" +
      "<FintConstant type=\"Fint\">1</FintConstant>" +
      "</lowerBound>" +
      "<upperBound>" +
      "<FintConstant type=\"Fint\">10</FintConstant>" +
      "</upperBound>" +
      "</indexRange>" +
      "</FbasicType>";


  /**
   * Test for a simple integer type
   *
   * integer(kind=8)
   */
  @Test
  public void simpleIntegerTypeTest(){
    XbasicType b = XmlHelper.createXbasicTypeFromString(type1);
    assertNotNull(b);
    assertTrue(b.hasKind());
    assertNotNull(b.getKind());
    assertNotNull(b.getRef());
    assertNotNull(b.getType());
    assertNull(b.getLength());
    assertFalse(b.hasLength());
    assertFalse(b.isAllocatable());
    assertFalse(b.isArray());
    assertFalse(b.isExternal());
    assertFalse(b.isIntrinsic());
    assertFalse(b.isOptional());
    assertFalse(b.isParameter());
    assertFalse(b.isPointer());
    assertFalse(b.isPrivate());
    assertFalse(b.isPublic());
    assertFalse(b.isSave());
    assertFalse(b.isTarget());
    assertEquals(0, b.getDimensions());

    assertEquals("Fint", b.getRef());
    assertEquals("TYPE_NAME", b.getType());
    assertEquals("8", b.getKind().getValue());
  }

  /**
   * Test for a simple character declarations
   *
   * character(len=10)
   */
  @Test
  public void simpleCharTypeTest(){
    XbasicType b = XmlHelper.createXbasicTypeFromString(type2);
    assertNotNull(b);
    assertFalse(b.hasKind());
    assertNull(b.getKind());
    assertNotNull(b.getRef());
    assertNotNull(b.getType());
    assertTrue(b.hasLength());
    assertNotNull(b.getLength());
    assertFalse(b.isAllocatable());
    assertFalse(b.isArray());
    assertFalse(b.isExternal());
    assertFalse(b.isIntrinsic());
    assertFalse(b.isOptional());
    assertFalse(b.isParameter());
    assertFalse(b.isPointer());
    assertFalse(b.isPrivate());
    assertFalse(b.isPublic());
    assertFalse(b.isSave());
    assertFalse(b.isTarget());
    assertEquals(0, b.getDimensions());

    assertEquals("Fcharacter", b.getRef());
    assertEquals("TYPE_NAME", b.getType());
    assertTrue(b.getLength().getExprModel().isIntConst());
    assertEquals("10", b.getLength().getExprModel().getIntConstant().getValue());
  }


  /**
   * Test for a more complex integer type with dimension
   *
   * integer dimension(10, 1:10)
   */
  @Test
  public void complexIntTypeTest() {
    XbasicType b = XmlHelper.createXbasicTypeFromString(type3);
    assertNotNull(b);
    assertFalse(b.hasKind());
    assertNull(b.getKind());
    assertNotNull(b.getRef());
    assertNotNull(b.getType());
    assertNull(b.getLength());
    assertFalse(b.hasLength());
    assertFalse(b.isAllocatable());
    assertTrue(b.isArray());
    assertFalse(b.isExternal());
    assertFalse(b.isIntrinsic());
    assertFalse(b.isOptional());
    assertFalse(b.isParameter());
    assertFalse(b.isPointer());
    assertFalse(b.isPrivate());
    assertFalse(b.isPublic());
    assertFalse(b.isSave());
    assertFalse(b.isTarget());
    assertEquals(2, b.getDimensions());
    assertNotNull(b.getDimensions(0));
    assertNotNull(b.getDimensions(1));
    assertNull(b.getDimensions(2));

    Xindex dim0 = b.getDimensions(0);
    Xindex dim1 = b.getDimensions(1);

    assertTrue(dim0.isArrayIndex());
    assertTrue(dim1.isIndexRange());

    XarrayIndex arrayIndex = (XarrayIndex)dim0;
    assertNotNull(arrayIndex.getExprModel());
    assertTrue(arrayIndex.getExprModel().isIntConst());
    assertEquals("10", arrayIndex.getExprModel().getIntConstant().getValue());

    XindexRange indexRange = (XindexRange)dim1;
    assertNotNull(indexRange.getLowerBound());
    assertNotNull(indexRange.getUpperBound());
    assertTrue(indexRange.getLowerBound().getExprModel().isIntConst());
    assertEquals("1", indexRange.getLowerBound().getValue());
    assertTrue(indexRange.getUpperBound().getExprModel().isIntConst());
    assertEquals("10", indexRange.getUpperBound().getValue());

    assertEquals("Fint", b.getRef());
    assertEquals("TYPE_NAME", b.getType());
  }
}
