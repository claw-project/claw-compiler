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
public class XintentTest {

  @Test
  public void ctorTest() {
    assertEquals(Xintent.IN, Xintent.fromString("in"));
    assertEquals(Xintent.IN, Xintent.fromString("IN"));

    assertEquals(Xintent.OUT, Xintent.fromString("out"));
    assertEquals(Xintent.OUT, Xintent.fromString("OUT"));

    assertEquals(Xintent.INOUT, Xintent.fromString("inout"));
    assertEquals(Xintent.INOUT, Xintent.fromString("INOUT"));

    assertEquals(Xintent.INOUT, Xintent.fromString("inOUT"));

    assertEquals(Xintent.NONE, Xintent.fromString(null));
    assertEquals(Xintent.NONE, Xintent.fromString(""));

    assertEquals(Xname.INTENT_IN, Xintent.IN.toString());
    assertEquals(Xname.INTENT_OUT, Xintent.OUT.toString());
    assertEquals(Xname.INTENT_INOUT, Xintent.INOUT.toString());
    assertEquals("", Xintent.NONE.toString());
    assertEquals("", Xintent.ANY.toString());
  }

  @Test
  public void checkTest() {
    Xintent intent1 = Xintent.IN;
    Xintent intent2 = Xintent.OUT;
    Xintent intent3 = Xintent.INOUT;
    Xintent intent4 = Xintent.NONE;
    Xintent any = Xintent.ANY;

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
}
