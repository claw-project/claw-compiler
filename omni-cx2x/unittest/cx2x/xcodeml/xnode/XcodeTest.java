/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test features of Xcode class.
 *
 * @author clementval
 */
public class XcodeTest {

  @Test
  public void stringToEnumTest() {
    for(Xcode opcode : Xcode.values()) {
      String rep = opcode.toString();
      Xcode code = Xcode.fromString(rep);
      assertEquals(opcode, code);
    }
  }
}
