/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import helper.XmlHelper;
import org.junit.Test;

import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertNotNull;

/**
 * Test methods of the XelementHelper class
 *
 * @author clementval
 */
public class XelementHelperTest {


  @Test
  public void createBinaryExprTest(){
    try {
      XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
      XbinaryExpr expr =
          XelementHelper.createEmpty(XelementName.LOG_EQ_EXPR, xcodeml);
      assertNotNull(expr);
    } catch (IllegalTransformationException ex){
      fail();
    }
  }
}
