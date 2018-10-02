/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Test methods of the FortranType class.
 *
 * @author clementval
 */
public class FortranTypeTest {

  @Test
  public void isBuiltInTypeTest() {
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_INT));
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_REAL));
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_COMPLEX));
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_LOGICAL));
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_CHAR));
    assertTrue(FortranType.isBuiltInType(Xname.TYPE_F_VOID));
    assertFalse(FortranType.isBuiltInType(null));
    assertFalse(FortranType.isBuiltInType(""));
    assertFalse(FortranType.isBuiltInType(FortranType.INTEGER.generateHash()));
  }

  @Test
  public void isOfTypeTest() {
    assertTrue(FortranType.INTEGER.
        isOfType(FortranType.INTEGER.generateHash()));
    assertTrue(FortranType.REAL.isOfType(FortranType.REAL.generateHash()));
    assertFalse(FortranType.REAL.isOfType(null));
    assertFalse(FortranType.REAL.isOfType(""));
  }
}
