/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xnode.Xnode;
import helper.TestConstant;
import helper.XmlHelper;
import org.junit.Test;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import static junit.framework.TestCase.fail;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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

  @Test
  public void xpathIntersectTest(){
    File f = new File(TestConstant.TEST_PROGRAM);
    assertTrue(f.exists());
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_PROGRAM);
    assertNotNull(xcodeml);

    List<Xnode> pragmas = XelementHelper.findAllPragmas(xcodeml);
    assertEquals(4, pragmas.size());

    Xnode loopHoistStart = new Xnode(pragmas.get(1).getElement());
    assertNotNull(loopHoistStart);
    assertTrue(loopHoistStart.getValue().contains("loop-hoist"));
    Xnode loopHoistEnd = new Xnode(pragmas.get(2).getElement());
    assertNotNull(loopHoistEnd);
    assertTrue(loopHoistEnd.getValue().contains("end loop-hoist"));

    List<Xnode> stmts =
        XelementHelper.findDoStatement(loopHoistStart, loopHoistEnd,
        Arrays.asList("j", "i"));

    assertEquals(3, stmts.size());


  }
}
