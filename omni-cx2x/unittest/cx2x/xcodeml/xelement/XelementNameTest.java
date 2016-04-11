/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test features of the XelementName class
 *
 * @author clementval
 */
public class XelementNameTest {


  @Test
  public void binaryExprSet(){
    assertTrue(XelementName.isBinaryExprTag(XelementName.DIV_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.F_CONCAT_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.F_POWER_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_AND_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_EQ_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_EQV_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_GE_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_GT_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_LE_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_LT_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_NEQ_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_NEWV_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.LOG_OR_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.MINUS_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.MUL_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.PLUS_EXPR));
    assertTrue(XelementName.isBinaryExprTag(XelementName.USER_BINARY_EXPR));

    assertFalse(XelementName.isBinaryExprTag(XelementName.UNARY_MINUS_EXPR));
    assertFalse(XelementName.isBinaryExprTag(XelementName.USER_UNARY_EXPR));
    assertFalse(XelementName.isBinaryExprTag(XelementName.LOG_NOT_EXPR));
  }


}
