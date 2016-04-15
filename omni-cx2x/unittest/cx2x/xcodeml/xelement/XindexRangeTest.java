/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test features of the XindexRange class. 
 *
 * @author clementval
 */
public class XindexRangeTest {

  private static final String idxRange1 =
      "<indexRange>" +
      "<lowerBound>" +
      "<FintConstant type=\"Fint\">1</FintConstant>" +
      "</lowerBound>" +
      "<upperBound>" +
      "<FintConstant type=\"Fint\">10</FintConstant>" +
      "</upperBound>" +
      "<step>" +
      "<FintConstant type=\"Fint\">1</FintConstant>" +
      "</step>" +
      "</indexRange>";

  @Test
  public void simpleIndexRangeTest(){
    XindexRange idx = XmlHelper.createXindexRange(idxRange1);
    assertNotNull(idx);
    assertNotNull(idx.getLowerBound());
    assertNotNull(idx.getUpperBound());
    assertNotNull(idx.getStep());
    assertTrue(idx.getLowerBound().getExprModel().isIntConst());
    assertTrue(idx.getUpperBound().getExprModel().isIntConst());
    assertTrue(idx.getStep().getExprModel().isIntConst());

    assertEquals("1",
        idx.getLowerBound().getExprModel().getIntConstant().getValue());
    assertEquals("10",
        idx.getUpperBound().getExprModel().getIntConstant().getValue());
    assertEquals("1",
        idx.getStep().getExprModel().getIntConstant().getValue());
  }

  @Test
  public void createTest(){
    try {
      XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
      Xvar var = Xvar.create("dummy", "array1", Xscope.LOCAL, xcodeml);
      XindexRange range = XindexRange.createAssumedShapeRange(xcodeml, var, 0);
      assertFalse(range.isAssumedShape());
      assertNotNull(range.getLowerBound());

      assertNotNull(range.getLowerBound().getExprModel());
      assertTrue(range.getLowerBound().getExprModel().isIntConst());
      assertNotNull(range.getLowerBound().getExprModel().getIntConstant());
      assertEquals("0", range.getLowerBound().getExprModel().getIntConstant().
          getValue());

      assertNotNull(range.getUpperBound());
      assertTrue(range.getUpperBound().getExprModel().isFctCall());
      assertNotNull(range.getUpperBound().getExprModel().getFctCall());
      assertTrue(range.getUpperBound().getExprModel().getFctCall().
          isIntrinsic());
      assertNotNull(range.getUpperBound().getExprModel().getFctCall().
          getName());
      assertEquals(XelementName.INTRINSIC_SIZE,
          range.getUpperBound().getExprModel().getFctCall().getName().
              getValue());
      assertEquals(2, range.getUpperBound().getExprModel().getFctCall().
          getArgumentsTable().count());
      assertNotNull(range.getUpperBound().getExprModel().getFctCall().
          getArgumentsTable().findArgument("array1"));
    } catch (IllegalTransformationException itex) {
      fail();
    }
  }
}
