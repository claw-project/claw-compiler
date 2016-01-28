/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import helper.XmlHelper;


/**
 * Test the features of the XloopIterationRange class
 *
 * @author clementval
 */
public class XloopIterationRangeTest {

  private static final String inducationVar1 =
      "<Var type=\"Fint\" scope=\"local\">i</Var>";
  private static final String indexRange1 =
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

  private static final String indexRange2 =
      "<indexRange>" +
          "<lowerBound>" +
          "<Var type=\"Fint\" scope=\"local\">istart</Var>" +
          "</lowerBound>" +
          "<upperBound>" +
          "<Var type=\"Fint\" scope=\"local\">iend</Var>" +
          "</upperBound>" +
          "<step>" +
          "<FintConstant type=\"Fint\">1</FintConstant>" +
          "</step>" +
          "</indexRange>";


  @Test
  public void basicLoopIterationRangeWithConstTest(){
    XloopIterationRange iterationRange =
        XmlHelper.createXloopIterationRange(inducationVar1, indexRange1);
    assertNotNull(iterationRange);
    assertNotNull(iterationRange.getInductionVar());
    assertNotNull(iterationRange.getIndexRange());
    assertNotNull(iterationRange.getIndexRange().getLowerBound());
    assertNotNull(iterationRange.getIndexRange().getUpperBound());
    assertNotNull(iterationRange.getIndexRange().getStep());
    assertEquals("i", iterationRange.getInductionVar().getValue());
    assertEquals("1", iterationRange.getIndexRange().getLowerBound().getValue());
    assertEquals("10", iterationRange.getIndexRange().getUpperBound().getValue());
    assertEquals("1", iterationRange.getIndexRange().getStep().getValue());
  }

  @Test
  public void basicLoopIterationRangeWithVarTest(){
    XloopIterationRange iterationRange =
        XmlHelper.createXloopIterationRange(inducationVar1, indexRange2);
    assertNotNull(iterationRange);
    assertNotNull(iterationRange.getInductionVar());
    assertNotNull(iterationRange.getIndexRange());
    assertNotNull(iterationRange.getIndexRange().getLowerBound());
    assertNotNull(iterationRange.getIndexRange().getUpperBound());
    assertNotNull(iterationRange.getIndexRange().getStep());
    assertEquals("i", iterationRange.getInductionVar().getValue());
    assertEquals("istart", iterationRange.getIndexRange().getLowerBound().getValue());
    assertEquals("iend", iterationRange.getIndexRange().getUpperBound().getValue());
    assertEquals("1", iterationRange.getIndexRange().getStep().getValue());
  }

}
