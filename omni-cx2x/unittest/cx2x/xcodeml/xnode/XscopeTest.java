/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test method of the Xintent class
 *
 * @author clementval
 */
public class XscopeTest {

  @Test
  public void ctorTest() {
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

}
