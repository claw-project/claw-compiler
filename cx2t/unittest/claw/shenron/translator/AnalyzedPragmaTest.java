/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.translator;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import static junit.framework.TestCase.*;

/**
 * Test methods of the AnalyzedPragma class.
 *
 * @author clementval
 */
public class AnalyzedPragmaTest {

  @Test
  public void ctorTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode p1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p1.setValue("acc parallel");
    Xnode p2 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p2.setValue("acc end parallel");

    AnalyzedPragma ap1 = new AnalyzedPragma(p1);
    assertFalse(ap1.isEndPragma());
    assertNotNull(ap1.getPragma());
    assertEquals("acc parallel", ap1.getPragma().value());

    ap1.setEndPragma();
    assertTrue(ap1.isEndPragma());

    AnalyzedPragma ap2 = new AnalyzedPragma();
    assertFalse(ap2.isEndPragma());
    assertNull(ap2.getPragma());
  }
}
