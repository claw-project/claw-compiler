package pragma;

import static org.junit.Assert.*;
import org.junit.Test;

import cx2x.translator.pragma.ClawPragma;

public class ClawPragmaTest {

  @Test
  public void isValidTest() {
    // loop-fusion
    assertTrue(ClawPragma.isValid("claw loop-fusion"));
    assertTrue(ClawPragma.isValid("claw loop-fusion group(g1)"));
    assertTrue(ClawPragma.isValid("claw loop-fusion group( g1 )"));
    assertTrue(ClawPragma.isValid("claw loop-fusion group ( g1   ) "));
    assertFalse(ClawPragma.isValid("claw loop-fusiongroup(g1)"));
    assertFalse(ClawPragma.isValid("claw loop-fusion group"));
    assertFalse(ClawPragma.isValid("claw loop-fusion (i,j,k)"));
    assertFalse(ClawPragma.isValid("claw loop-fusion group()"));
    assertFalse(ClawPragma.isValid("claw loop-fusion group(   )"));

    // loop-interchange
    assertTrue(ClawPragma.isValid("claw loop-interchange"));
    assertTrue(ClawPragma.isValid("claw loop-interchange (i,j,k)"));
    assertTrue(ClawPragma.isValid("claw loop-interchange (  i,j,k  ) "));
    assertFalse(ClawPragma.isValid("claw loop-interchange ()"));
    assertFalse(ClawPragma.isValid("claw loop-interchange (  )"));

    // loop-extract
    assertTrue(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map(value1:i) map(value2:i)"));
    assertTrue(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map(value1,value2:i)"));
    assertTrue(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map(value1, value2:i)"));
    assertFalse(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map(value1, value2)"));
    assertFalse(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map(:i)"));
    assertFalse(ClawPragma.isValid("claw loop-extract range(i=istart,iend)"));
    assertFalse(ClawPragma.isValid("claw loop-extract map(value1:i)"));
    assertFalse(ClawPragma.isValid("claw loop-extract range() map(value1:i)"));
    assertFalse(ClawPragma.isValid("claw loop-extract range(i=istart,iend) map()"));
    assertFalse(ClawPragma.isValid("claw loop-extract range() map()"));
    assertFalse(ClawPragma.isValid("claw loop-extract range map"));


    // remove
    assertTrue(ClawPragma.isValid("claw remove"));
    assertTrue(ClawPragma.isValid("claw end remove"));

    // invalid dummy directives
    assertFalse(ClawPragma.isValid("claw"));
    assertFalse(ClawPragma.isValid("claw dummy-directive"));


  }
}
