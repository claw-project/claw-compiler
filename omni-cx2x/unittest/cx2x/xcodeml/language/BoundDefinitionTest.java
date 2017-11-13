/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import static junit.framework.TestCase.assertFalse;
import static org.junit.Assert.*;

/**
 * Test methods of the BoundDefinition class.
 *
 * @author clementval
 */
public class BoundDefinitionTest {

  @Test
  public void ctorTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    BoundDefinition lowerBound =
        new BoundDefinition("1", BoundDefinition.BoundType.LOWER);
    BoundDefinition upperBound =
        new BoundDefinition("nend", BoundDefinition.BoundType.LOWER);

    assertFalse(lowerBound.isVar());
    assertTrue(upperBound.isVar());

    assertEquals(1, lowerBound.getIntValue());
    assertEquals("nend", upperBound.getValue());

    Xnode lowerNode = lowerBound.generate(xcodeml);
    assertNotNull(lowerNode);
    assertNotNull(lowerNode.firstChild());
    assertEquals(Xcode.FINTCONSTANT, lowerNode.firstChild().opcode());
    assertEquals("1", lowerNode.firstChild().value());
    Xnode upperNode = upperBound.generate(xcodeml);
    assertNotNull(upperNode);
    assertNotNull(upperNode.firstChild());
    assertEquals(Xcode.VAR, upperNode.firstChild().opcode());
    assertEquals("nend", upperNode.firstChild().value());
  }
}
