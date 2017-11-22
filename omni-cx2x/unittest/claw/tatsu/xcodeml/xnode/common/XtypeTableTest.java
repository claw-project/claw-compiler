/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.*;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test features of the XtypeTable
 *
 * @author clementval
 */
public class XtypeTableTest {

  private static final String basicTypeHash = "C2307e50";
  private static final String fctTypeHash = "F23079f0";
  private static final String structTypeHash1 = "S7fd10b600b70";
  private static final String structTypeHash2 = "S7fd10b6012d0";
  private static final String basicTypeTable = "<typeTable>" +
      "<FbasicType type=\"" + basicTypeHash + "\" ref=\"Fcharacter\">" +
      "<len><FintConstant type=\"Fint\">30</FintConstant></len>" +
      "</FbasicType>" +
      "<FfunctionType type=\"" + fctTypeHash +
      "\" return_type=\"Fvoid\" is_program=\"true\"/>" +
      "<FstructType type=\"" + structTypeHash1 + "\">" +
      "<symbols></symbols></FstructType>" +
      "<FstructType type=\"" + structTypeHash2 + "\" " +
      "extends=\"" + structTypeHash1 + "\">" +
      "<symbols></symbols></FstructType></typeTable>";

  @Test
  public void basicTypeTableTest() {
    // Base typeTable tests
    XtypeTable typeTable = XmlHelper.createXtypeTableFromString(basicTypeTable);
    assertNotNull(typeTable);
    assertEquals(4, typeTable.size());
    assertEquals(4, typeTable.values().size());

    // FbasicType test
    assertFalse(typeTable.isBasicType((String) null));
    assertFalse(typeTable.isBasicType(""));

    assertTrue(typeTable.hasType(basicTypeHash));
    assertTrue(typeTable.isBasicType(basicTypeHash));
    FbasicType type1 = typeTable.getBasicType(basicTypeHash);
    assertNotNull(type1);
    assertTrue(typeTable.isBasicType(basicTypeHash));
    assertTrue(typeTable.isBasicType(type1));
    assertFalse(type1.hasIntent());
    assertEquals(Intent.NONE, type1.getIntent());
    assertFalse(type1.hasKind());
    assertTrue(type1.hasLength());
    assertEquals(Xname.TYPE_F_CHAR, type1.getRef());
    assertTrue(type1.getLength().child(0).opcode() == Xcode.F_INT_CONSTANT);
    assertEquals("30", type1.getLength().child(0).value());

    assertNull(typeTable.getFunctionType(basicTypeHash));
    assertNull(typeTable.getFunctionType(type1));
    assertNull(typeTable.getStructType(basicTypeHash));
    assertNull(typeTable.getStructType(type1));

    // FfunctionType test
    assertTrue(typeTable.hasType(fctTypeHash));
    assertTrue(typeTable.isFunctionType(fctTypeHash));
    FfunctionType type2 = typeTable.getFunctionType(fctTypeHash);
    assertNotNull(type2);
    assertTrue(typeTable.isFunctionType(fctTypeHash));
    assertTrue(typeTable.isFunctionType(type2));
    assertEquals(Xname.TYPE_F_VOID, type2.getReturnType());
    assertTrue(type2.isProgram());
    assertFalse(type2.isInternal());
    assertFalse(type2.isRecursive());
    assertNull(type2.getResultName());

    assertNull(typeTable.getBasicType(fctTypeHash));
    assertNull(typeTable.getStructType(fctTypeHash));
    assertNull(typeTable.getBasicType(type2));
    assertNull(typeTable.getStructType(type2));

    // FstructType tests
    assertTrue(typeTable.hasType(structTypeHash1));
    assertTrue(typeTable.isStructType(structTypeHash1));
    assertTrue(typeTable.hasType(structTypeHash2));
    assertTrue(typeTable.isStructType(structTypeHash2));
    FstructType structType1 = typeTable.getStructType(structTypeHash1);
    FstructType structType2 = typeTable.getStructType(structTypeHash2);
    assertNotNull(structType1);
    assertNotNull(structType2);
    assertTrue(typeTable.isStructType(structTypeHash1));
    assertTrue(typeTable.isStructType(structTypeHash2));
    assertTrue(typeTable.isStructType(structType1));
    assertTrue(typeTable.isStructType(structType2));
    assertFalse(structType1.isExtend());
    assertTrue(structType2.isExtend());
    assertEquals(structTypeHash1, structType2.getExtend());
    assertFalse(structType1.isSequence());
    assertFalse(structType1.isInternalPrivate());
    assertFalse(structType1.isAbstract());
    assertFalse(structType1.isPrivate());
    assertFalse(structType1.isPublic());
    assertNull(structType1.getBind());

    assertNull(typeTable.getBasicType(structTypeHash1));
    assertNull(typeTable.getFunctionType(structTypeHash1));
    assertNull(typeTable.getBasicType(structTypeHash2));
    assertNull(typeTable.getFunctionType(structTypeHash2));
    assertNull(typeTable.getBasicType(structType1));
    assertNull(typeTable.getFunctionType(structType1));
    assertNull(typeTable.getBasicType(structType2));
    assertNull(typeTable.getFunctionType(structType2));

    // Clone
    XtypeTable clone = typeTable.cloneNode();
    assertNotNull(clone);
    assertNotEquals(typeTable.element(), clone.element());
    assertEquals(4, clone.values().size());
    assertEquals(4, clone.size());
  }


  @Test
  public void hashTest() {
    XtypeTable typeTable = XmlHelper.createXtypeTableFromString(basicTypeTable);
    assertNotNull(typeTable);

    assertEquals("", typeTable.generateHash(null));

    String intHash = typeTable.generateHash(FortranType.INTEGER);
    assertEquals(13, intHash.length());
    assertTrue(intHash.startsWith("I"));

    String realHash = typeTable.generateHash(FortranType.REAL);
    assertEquals(13, realHash.length());
    assertTrue(realHash.startsWith("R"));

    String complexHash = typeTable.generateHash(FortranType.COMPLEX);
    assertEquals(13, complexHash.length());
    assertTrue(complexHash.startsWith("P"));

    String logicalHash = typeTable.generateHash(FortranType.LOGICAL);
    assertEquals(13, logicalHash.length());
    assertTrue(logicalHash.startsWith("L"));

    String charHash = typeTable.generateHash(FortranType.CHARACTER);
    assertEquals(13, charHash.length());
    assertTrue(charHash.startsWith("C"));
  }
}
