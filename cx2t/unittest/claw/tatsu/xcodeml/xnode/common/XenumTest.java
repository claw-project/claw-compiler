/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.TatsuConstant;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Testing methods from various enum class. Xattr, FortranType, Xcode, Intent,
 * Xscope.
 *
 * @author clementval
 */
public class XenumTest {

  @Test
  public void xAttrStringToEnumTest() {
    for(Xattr attrCode : Xattr.values()) {
      String rep = attrCode.toString();
      Xattr attr = Xattr.fromString(rep);
      assertEquals(attrCode, attr);
    }
    assertNull(Xattr.fromString(null));
    assertNull(Xattr.fromString("dummy"));
  }

  @Test
  public void xIntentCtorTest() {
    assertEquals(Intent.IN, Intent.fromString("in"));
    assertEquals(Intent.IN, Intent.fromString("IN"));

    assertEquals(Intent.OUT, Intent.fromString("out"));
    assertEquals(Intent.OUT, Intent.fromString("OUT"));

    assertEquals(Intent.INOUT, Intent.fromString("inout"));
    assertEquals(Intent.INOUT, Intent.fromString("INOUT"));

    assertEquals(Intent.INOUT, Intent.fromString("inOUT"));

    assertEquals(Intent.NONE, Intent.fromString(null));
    assertEquals(Intent.NONE, Intent.fromString(""));

    assertEquals(Xname.INTENT_IN, Intent.IN.toString());
    assertEquals(Xname.INTENT_OUT, Intent.OUT.toString());
    assertEquals(Xname.INTENT_INOUT, Intent.INOUT.toString());
    assertEquals("", Intent.NONE.toString());
    assertEquals("", Intent.ANY.toString());
  }

  @Test
  public void xIntentCheckTest() {
    Intent intent1 = Intent.IN;
    Intent intent2 = Intent.OUT;
    Intent intent3 = Intent.INOUT;
    Intent intent4 = Intent.NONE;
    Intent any = Intent.ANY;

    assertTrue(intent1.isIntentIn());
    assertFalse(intent2.isIntentIn());
    assertTrue(intent3.isIntentIn());
    assertFalse(intent4.isIntentIn());

    assertFalse(intent1.isIntentOut());
    assertTrue(intent2.isIntentOut());
    assertTrue(intent3.isIntentOut());
    assertFalse(intent4.isIntentOut());

    assertTrue(intent1.isIntent());
    assertTrue(intent2.isIntent());
    assertTrue(intent3.isIntent());
    assertFalse(intent4.isIntent());

    assertTrue(intent1.isCompatible(intent1));
    assertFalse(intent1.isCompatible(intent2));
    assertTrue(intent1.isCompatible(intent3));
    assertFalse(intent1.isCompatible(intent4));
    assertTrue(intent1.isCompatible(any));

    assertFalse(intent2.isCompatible(intent1));
    assertTrue(intent2.isCompatible(intent2));
    assertTrue(intent2.isCompatible(intent3));
    assertFalse(intent2.isCompatible(intent4));
    assertTrue(intent2.isCompatible(any));

    assertTrue(intent3.isCompatible(intent1));
    assertTrue(intent3.isCompatible(intent2));
    assertTrue(intent3.isCompatible(intent3));
    assertFalse(intent3.isCompatible(intent4));
    assertTrue(intent3.isCompatible(any));

    assertFalse(intent4.isCompatible(intent1));
    assertFalse(intent4.isCompatible(intent2));
    assertFalse(intent4.isCompatible(intent3));
    assertTrue(intent4.isCompatible(intent4));
    assertTrue(intent4.isCompatible(any));

    assertTrue(any.isCompatible(intent1));
    assertTrue(any.isCompatible(intent2));
    assertTrue(any.isCompatible(intent3));
    assertTrue(any.isCompatible(intent4));
    assertTrue(any.isCompatible(any));

    assertFalse(intent1.isCompatible(null));
    assertFalse(intent2.isCompatible(null));
    assertFalse(intent3.isCompatible(null));
    assertFalse(intent4.isCompatible(null));
    assertFalse(any.isCompatible(null));
  }

  @Test
  public void xScopeCctorTest() {
    assertEquals(Xscope.LOCAL, Xscope.fromString("local"));
    assertEquals(Xscope.GLOBAL, Xscope.fromString("global"));
    assertEquals(Xscope.PARAM, Xscope.fromString("param"));
    assertEquals(Xscope.LOCAL, Xscope.fromString("LOCAL"));
    assertEquals(Xscope.GLOBAL, Xscope.fromString("GLOBAL"));
    assertEquals(Xscope.PARAM, Xscope.fromString("PARAM"));

    assertNull(Xscope.fromString(""));
    assertNull(Xscope.fromString(null));

    assertEquals(Xname.SCOPE_LOCAL, Xscope.LOCAL.toString());
    assertEquals(Xname.SCOPE_GLOBAL, Xscope.GLOBAL.toString());
    assertEquals(Xname.SCOPE_PARAM, Xscope.PARAM.toString());
  }

  @Test
  public void xCodeTypeCtorTest() {
    assertEquals(FortranType.NONE, FortranType.fromString(null));
    assertEquals(FortranType.NONE, FortranType.fromString(""));
    assertEquals(FortranType.INTEGER,
        FortranType.fromString(Xname.TYPE_F_INT));
    assertEquals(FortranType.REAL,
        FortranType.fromString(Xname.TYPE_F_REAL));
    assertEquals(FortranType.COMPLEX,
        FortranType.fromString(Xname.TYPE_F_COMPLEX));
    assertEquals(FortranType.LOGICAL,
        FortranType.fromString(Xname.TYPE_F_LOGICAL));
    assertEquals(FortranType.CHARACTER,
        FortranType.fromString(Xname.TYPE_F_CHAR));
    assertEquals(FortranType.VOID,
        FortranType.fromString(Xname.TYPE_F_VOID));

    assertEquals(Xname.TYPE_F_INT, FortranType.INTEGER.toString());
    assertEquals(Xname.TYPE_F_REAL, FortranType.REAL.toString());
    assertEquals(Xname.TYPE_F_COMPLEX, FortranType.COMPLEX.toString());
    assertEquals(Xname.TYPE_F_LOGICAL, FortranType.LOGICAL.toString());
    assertEquals(Xname.TYPE_F_CHAR, FortranType.CHARACTER.toString());
    assertEquals(Xname.TYPE_F_VOID, FortranType.VOID.toString());
  }

  @Test
  public void xStorageClassCtorTest() {
    assertEquals(XstorageClass.NONE, XstorageClass.fromString(null));
    assertEquals(XstorageClass.NONE, XstorageClass.fromString(""));
    assertEquals(XstorageClass.AUTO,
        XstorageClass.fromString(Xname.SCLASS_AUTO));
    assertEquals(XstorageClass.F_LOCAL,
        XstorageClass.fromString(Xname.SCLASS_F_LOCAL));
    assertEquals(XstorageClass.F_PARAM,
        XstorageClass.fromString(Xname.SCLASS_F_PARAM));
    assertEquals(XstorageClass.F_FUNC,
        XstorageClass.fromString(Xname.SCLASS_F_FUNC));
    assertEquals(XstorageClass.EXTERN,
        XstorageClass.fromString(Xname.SCLASS_EXTERN));
    assertEquals(XstorageClass.EXTERN_DEF,
        XstorageClass.fromString(Xname.SCLASS_EXTERN_DEF));
    assertEquals(XstorageClass.LABEL,
        XstorageClass.fromString(Xname.SCLASS_LABEL));
    assertEquals(XstorageClass.PARAM,
        XstorageClass.fromString(Xname.SCLASS_PARAM));

    assertEquals(Xname.SCLASS_AUTO, XstorageClass.AUTO.toString());
    assertEquals(Xname.SCLASS_EXTERN, XstorageClass.EXTERN.toString());
    assertEquals(Xname.SCLASS_EXTERN_DEF, XstorageClass.EXTERN_DEF.toString());
    assertEquals(Xname.SCLASS_F_LOCAL, XstorageClass.F_LOCAL.toString());
    assertEquals(Xname.SCLASS_F_FUNC, XstorageClass.F_FUNC.toString());
    assertEquals(Xname.SCLASS_F_PARAM, XstorageClass.F_PARAM.toString());
    assertEquals(Xname.SCLASS_LABEL, XstorageClass.LABEL.toString());
    assertEquals(Xname.SCLASS_PARAM, XstorageClass.PARAM.toString());
  }

  @Test
  public void xCodeStringToEnumTest() {
    for(Xcode opcode : Xcode.values()) {
      String rep = opcode.toString();
      Xcode code = Xcode.fromString(rep);
      assertEquals(opcode, code);
    }
  }

  @Test
  public void insertionPositionTest() {
    assertEquals(InsertionPosition.BEFORE,
        InsertionPosition.fromString(null));
    assertEquals(InsertionPosition.BEFORE,
        InsertionPosition.fromString(""));
    assertEquals(InsertionPosition.BEFORE,
        InsertionPosition.fromString("dummy"));
    assertEquals(InsertionPosition.BEFORE,
        InsertionPosition.fromString("before"));
    assertEquals(InsertionPosition.IN_MIDDLE,
        InsertionPosition.fromString("middle"));
    assertEquals(InsertionPosition.AFTER,
        InsertionPosition.fromString("after"));

    assertEquals(TatsuConstant.BEFORE, InsertionPosition.BEFORE.toString());
    assertEquals(TatsuConstant.MIDDLE, InsertionPosition.IN_MIDDLE.toString());
    assertEquals(TatsuConstant.AFTER, InsertionPosition.AFTER.toString());
  }
}
