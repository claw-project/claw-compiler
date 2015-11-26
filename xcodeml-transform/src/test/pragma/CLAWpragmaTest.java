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

    

    // invalid dummy directives
    assertFalse(CLAWpragma.isValid("claw"));
    assertFalse(CLAWpragma.isValid("claw dummy-directive"));
  }
}
