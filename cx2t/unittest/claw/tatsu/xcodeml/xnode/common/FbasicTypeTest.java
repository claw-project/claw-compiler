/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import helper.XmlHelper;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.*;

/**
 * @author clementval
 */
public class FbasicTypeTest {

  private static final String type1 = "<FbasicType type=\"TYPE_NAME\" " +
      "ref=\"Fint\"><kind>8</kind></FbasicType>";
  private static final String type2 = "<FbasicType type=\"TYPE_NAME\" " +
      "ref=\"Fcharacter\"> <len>" +
      "<FintConstant type=\"Fint\">10</FintConstant> </len></FbasicType>";
  private static final String type3 = "<FbasicType type=\"TYPE_NAME\" " +
      "ref=\"Fint\"><arrayIndex>" +
      "<FintConstant type=\"Fint\">10</FintConstant></arrayIndex>" +
      "<indexRange><lowerBound><FintConstant type=\"Fint\">1</FintConstant>" +
      "</lowerBound><upperBound>" +
      "<FintConstant type=\"Fint\">10</FintConstant></upperBound>" +
      "</indexRange></FbasicType>";
  private static final String type4 = "<FbasicType type=\"TYPE_NAME\" " +
      "ref=\"Fint\">" +
      "<indexRange is_assumed_shape=\"true\"></indexRange>" +
      "<indexRange is_assumed_shape=\"true\"></indexRange>" +
      "</FbasicType>";

  /**
   * Test setter for FbasicType
   */
  @Test
  public void setterTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    FbasicType bt1 = new FbasicType(xcodeml.createNode(Xcode.F_BASIC_TYPE));
    FbasicType bt2 = new FbasicType(xcodeml.createNode(Xcode.F_BASIC_TYPE));
    String typeHash1 = xcodeml.getTypeTable().generateHash(FortranType.INTEGER);
    String typeHash2 = xcodeml.getTypeTable().generateHash(FortranType.INTEGER);
    bt1.setType(typeHash1);
    bt2.setType(typeHash2);
    bt1.setRef(typeHash2);
    bt2.setRef(Xname.TYPE_F_INT);
    assertEquals(Xname.TYPE_F_INT, bt2.getRef());
    assertEquals(typeHash2, bt1.getRef());
    assertFalse(bt1.hasIntent());
    bt1.setIntent(Intent.IN);
    assertTrue(bt1.hasIntent());
    assertEquals(Intent.IN, bt1.getIntent());
    bt1.removeAttribute(Xattr.INTENT);
    assertEquals(Intent.NONE, bt1.getIntent());
    assertFalse(bt1.hasIntent());
    assertFalse(bt1.isArray());
    assertFalse(bt2.isArray());

    bt1.setBooleanAttribute(Xattr.IS_ALLOCATABLE, true);
    assertTrue(bt1.isAllocatable());
    bt1.removeAttribute(Xattr.IS_ALLOCATABLE);
    assertFalse(bt1.isAllocatable());
  }

  /**
   * Test for a simple integer type
   *
   * integer(kind=8)
   */
  @Test
  public void simpleIntegerTypeTest() {
    FbasicType b = XmlHelper.createXbasicTypeFromString(type1);
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
    assertEquals("8", b.getKind().value());
  }

  /**
   * Test for a simple character declarations
   *
   * character(len=10)
   */
  @Test
  public void simpleCharTypeTest() {
    FbasicType b = XmlHelper.createXbasicTypeFromString(type2);
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
    assertSame(b.getLength().child(0).opcode(), Xcode.F_INT_CONSTANT);
    assertEquals("10", b.getLength().child(0).value());
  }

  /**
   * Test for a more complex integer type with dimension
   *
   * integer dimension(10, 1:10)
   */
  @Test
  public void complexIntTypeTest() {
    FbasicType b = XmlHelper.createXbasicTypeFromString(type3);
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

    Xnode dim0 = b.getDimensions(0);
    Xnode dim1 = b.getDimensions(1);

    assertSame(dim0.opcode(), Xcode.ARRAY_INDEX);
    assertSame(dim1.opcode(), Xcode.INDEX_RANGE);

    assertSame(dim0.child(0).opcode(), Xcode.F_INT_CONSTANT);
    assertEquals("10", dim0.child(0).value());

    assertNotNull(dim1.matchSeq(Xcode.LOWER_BOUND));
    assertNotNull(dim1.matchSeq(Xcode.UPPER_BOUND));
    assertSame(dim1.matchSeq(Xcode.LOWER_BOUND).child(0).opcode(),
        Xcode.F_INT_CONSTANT);
    assertEquals("1", dim1.matchSeq(Xcode.LOWER_BOUND).child(0).value());
    assertSame(dim1.matchSeq(Xcode.LOWER_BOUND).child(0).opcode(),
        Xcode.F_INT_CONSTANT);
    assertEquals("10", dim1.matchSeq(Xcode.UPPER_BOUND).child(0).value());

    assertEquals("Fint", b.getRef());
    assertEquals("TYPE_NAME", b.getType());

    assertEquals(2, b.getDimensions());
    assertFalse(b.isAllAssumedShape());
    b.removeDimension(Collections.singletonList(1));
    assertEquals(1, b.getDimensions());
    b.resetDimension();
    assertEquals(0, b.getDimensions());
    assertFalse(b.isArray());
    assertFalse(b.isAllAssumedShape());
  }

  @Test
  public void dimTest() {
    FbasicType b = XmlHelper.createXbasicTypeFromString(type3);
    b.removeDimension(Collections.<Integer>emptyList());
    assertEquals(0, b.getDimensions());
    assertFalse(b.isArray());
    assertFalse(b.isAllAssumedShape());

    FbasicType b2 = XmlHelper.createXbasicTypeFromString(type4);
    assertNotNull(b2);
    assertTrue(b2.isAllAssumedShape());

    assertNotNull(b2.toString());
    assertFalse(b2.toString().isEmpty());
  }

  @Test
  public void addDimensionTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.NONE);
    assertEquals(0, bt.getDimensions());
    assertFalse(bt.isArray());
    assertFalse(bt.isAllAssumedShape());
    Xnode d1 = xcodeml.createEmptyAssumedShaped();
    bt.addDimension(d1);
    assertEquals(1, bt.getDimensions());
    Xnode d2 = xcodeml.createEmptyAssumedShaped();
    bt.addDimension(d2);
    assertEquals(2, bt.getDimensions());
    assertTrue(bt.isAllAssumedShape());
    Xnode arrayIndex = xcodeml.createNode(Xcode.ARRAY_INDEX);
    arrayIndex.append(xcodeml.createIntConstant(10));
    bt.addDimension(arrayIndex, 0);
    assertEquals(3, bt.getDimensions());
    assertEquals(Xcode.ARRAY_INDEX, bt.getDimensions(0).opcode());
    assertEquals(Xcode.INDEX_RANGE, bt.getDimensions(1).opcode());
    assertEquals(Xcode.INDEX_RANGE, bt.getDimensions(2).opcode());
    assertTrue(bt.getDimensions(1).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE));
    assertTrue(bt.getDimensions(2).getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE));
    assertFalse(bt.isAllAssumedShape());
    assertTrue(bt.isArray());
  }

  @Test
  public void cloneTest() {
    FbasicType b = XmlHelper.createXbasicTypeFromString(type3);
    FbasicType b2 = b.cloneNode();
    assertEquals(b.isAllAssumedShape(), b2.isAllAssumedShape());
    assertEquals(b.isArray(), b2.isArray());
    assertEquals(b.isAllocatable(), b2.isAllocatable());
    assertEquals(b.isPointer(), b2.isPointer());
    assertEquals(b.isParameter(), b2.isParameter());
    assertEquals(b.isTarget(), b2.isTarget());
    assertEquals(b.isExternal(), b2.isExternal());
    assertEquals(b.isIntrinsic(), b2.isIntrinsic());
    assertEquals(b.isOptional(), b2.isOptional());
    assertEquals(b.isPrivate(), b2.isPrivate());
    assertEquals(b.isPublic(), b2.isPublic());
    assertEquals(b.isSave(), b2.isSave());
    assertEquals(b.hasIntent(), b2.hasIntent());
    assertEquals(b.hasKind(), b2.hasKind());
    assertEquals(b.hasLength(), b2.hasLength());
    assertEquals(b.getDimensions(), b2.getDimensions());
    assertEquals(b.getIntent(), b2.getIntent());
  }
}
