/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

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
    assertEquals(4, typeTable.count());

    // FbasicType test
    assertTrue(typeTable.hasType(basicTypeHash));
    XbasicType type1 = typeTable.getBasicType(basicTypeHash);
    assertNotNull(type1);
    assertTrue(typeTable.isBasicType(basicTypeHash));
    assertFalse(type1.hasIntent());
    assertEquals(Xintent.NONE, type1.getIntent());
    assertFalse(type1.hasKind());
    assertTrue(type1.hasLength());
    assertEquals(Xname.TYPE_F_CHAR, type1.getRef());
    assertTrue(type1.getLength().child(0).opcode() == Xcode.FINTCONSTANT);
    assertEquals("30", type1.getLength().child(0).value());

    // FfunctionType test
    assertTrue(typeTable.hasType(fctTypeHash));
    XfunctionType type2 = typeTable.getFunctionType(fctTypeHash);
    assertNotNull(type2);
    assertTrue(typeTable.isFunctionType(fctTypeHash));
    assertEquals(Xname.TYPE_F_VOID, type2.getReturnType());
    assertTrue(type2.isProgram());
    assertFalse(type2.isInternal());
    assertFalse(type2.isRecursive());
    assertNull(type2.getResultName());

    // FstructType tests
    assertTrue(typeTable.hasType(structTypeHash1));
    assertTrue(typeTable.hasType(structTypeHash2));
    XstructType structType1 = typeTable.getStructType(structTypeHash1);
    XstructType structType2 = typeTable.getStructType(structTypeHash2);
    assertNotNull(structType1);
    assertNotNull(structType2);
    assertTrue(typeTable.isStructType(structTypeHash1));
    assertTrue(typeTable.isStructType(structTypeHash2));
    assertFalse(structType1.isExtend());
    assertTrue(structType2.isExtend());
    assertEquals(structTypeHash1, structType2.getExtend());
  }
}
