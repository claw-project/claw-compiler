/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;
import org.junit.Test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Test methods of the XcodeType class.
 *
 * @author clementval
 */
public class XcodeTypeTest {

  @Test
  public void isBuiltInTypeTest() {
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_INT));
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_REAL));
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_COMPLEX));
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_LOGICAL));
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_CHAR));
    assertTrue(XcodeType.isBuiltInType(Xname.TYPE_F_VOID));
    assertFalse(XcodeType.isBuiltInType(null));
    assertFalse(XcodeType.isBuiltInType(""));
    assertFalse(XcodeType.isBuiltInType(XcodeType.INTEGER.generateHash()));
  }

  @Test
  public void isOfTypeTest() {
    assertTrue(XcodeType.INTEGER.isOfType(XcodeType.INTEGER.generateHash()));
    assertTrue(XcodeType.REAL.isOfType(XcodeType.REAL.generateHash()));
    assertFalse(XcodeType.REAL.isOfType(null));
    assertFalse(XcodeType.REAL.isOfType(""));
  }
}
