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

  private static final String basicTypeTable =
      "<typeTable>" +
          "<FbasicType type=\"C2307e50\" ref=\"Fcharacter\">" +
          "<len>" +
          "<FintConstant type=\"Fint\">30</FintConstant>" +
          "</len>" +
          "</FbasicType>" +
          "<FfunctionType type=\"F23079f0\" return_type=\"Fvoid\" is_program=\"true\"/>" +
          "</typeTable>";


  @Test
  public void basicTypeTableTest() {
    XtypeTable typeTable = XmlHelper.createXtypeTableFromString(basicTypeTable);
    assertNotNull(typeTable);
    assertEquals(2, typeTable.count());
    assertTrue(typeTable.hasType("C2307e50"));
    Xtype type1 = typeTable.get("C2307e50");
    assertNotNull(type1);
    assertTrue(type1 instanceof XbasicType);
    XbasicType bType1 = (XbasicType) type1;
    assertFalse(bType1.hasIntent());
    assertEquals(Xintent.NONE, bType1.getIntent());
    assertFalse(bType1.hasKind());
    assertTrue(bType1.hasLength());
    assertEquals(Xname.TYPE_F_CHAR, bType1.getRef());
    assertTrue(bType1.getLength().child(0).opcode() == Xcode.FINTCONSTANT);
    assertEquals("30", bType1.getLength().child(0).value());

    assertTrue(typeTable.hasType("F23079f0"));
    Xtype type2 = typeTable.get("F23079f0");
    assertNotNull(type2);
    assertTrue(type2 instanceof XfunctionType);
    XfunctionType fType2 = (XfunctionType) type2;
    assertEquals(Xname.TYPE_F_VOID, fType2.getReturnType());
    assertTrue(fType2.isProgram());
    assertFalse(fType2.isInternal());
    assertFalse(fType2.isRecursive());
    assertNull(fType2.getResultName());
  }
}
