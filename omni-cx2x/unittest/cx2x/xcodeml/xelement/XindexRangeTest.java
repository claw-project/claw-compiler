/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
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
}
