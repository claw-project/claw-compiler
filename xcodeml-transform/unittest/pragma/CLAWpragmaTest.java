package pragma;

import static org.junit.Assert.*;
import org.junit.Test;

import x2x.translator.pragma.CLAWpragma;

public class CLAWpragmaTest {

  @Test
  public void isValidTest() {
    // loop-fusion
    assertTrue(CLAWpragma.isValid("claw loop-fusion"));
    assertTrue(CLAWpragma.isValid("claw loop-fusion group(g1)"));
    assertTrue(CLAWpragma.isValid("claw loop-fusion group( g1 )"));
    assertTrue(CLAWpragma.isValid("claw loop-fusion group ( g1   ) "));
    assertFalse(CLAWpragma.isValid("claw loop-fusiongroup(g1)"));
    assertFalse(CLAWpragma.isValid("claw loop-fusion group"));
    assertFalse(CLAWpragma.isValid("claw loop-fusion (i,j,k)"));
    assertFalse(CLAWpragma.isValid("claw loop-fusion group()"));
    assertFalse(CLAWpragma.isValid("claw loop-fusion group(   )"));

    // loop-interchange
    assertTrue(CLAWpragma.isValid("claw loop-interchange"));
    assertTrue(CLAWpragma.isValid("claw loop-interchange (i,j,k)"));
    assertTrue(CLAWpragma.isValid("claw loop-interchange (  i,j,k  ) "));
    assertFalse(CLAWpragma.isValid("claw loop-interchange ()"));
    assertFalse(CLAWpragma.isValid("claw loop-interchange (  )"));

    // loop-extract
    assertTrue(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map(value1:i) map(value2:i)"));
    assertTrue(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map(value1,value2:i)"));
    assertTrue(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map(value1, value2:i)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map(value1, value2)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map(:i)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range(i=istart,iend)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract map(value1:i)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range() map(value1:i)"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range(i=istart,iend) map()"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range() map()"));
    assertFalse(CLAWpragma.isValid("claw loop-extract range map"));


    // remove
    assertTrue(CLAWpragma.isValid("claw remove"));
    assertTrue(CLAWpragma.isValid("claw end remove"));

    // invalid dummy directives
    assertFalse(CLAWpragma.isValid("claw"));
    assertFalse(CLAWpragma.isValid("claw dummy-directive"));


  }
}
