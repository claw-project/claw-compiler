/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.pragma;

import cx2x.xcodeml.xelement.XloopIterationRange;
import helper.XmlHelper;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Test the features of the ClawRange class
 *
 * @author clementval
 */
public class ClawRangeTest {

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
  public void compareWithXloopIterationRangeTest(){
    XloopIterationRange iterationRange1 =
        XmlHelper.createXloopIterationRange(inducationVar1, indexRange1);
    assertNotNull(iterationRange1);
    XloopIterationRange iterationRange2 =
        XmlHelper.createXloopIterationRange(inducationVar1, indexRange2);
    assertNotNull(iterationRange1);

    ClawRange range1 = new ClawRange("i", "1", "10", "1");
    assertTrue(range1.equals(iterationRange1));
    assertFalse(range1.equals(iterationRange2));

    ClawRange range2 = new ClawRange("i", "1", "10", "2");
    assertFalse(range2.equals(iterationRange1));
    assertFalse(range2.equals(iterationRange2));

    ClawRange range3 = new ClawRange("i", "1", "11", "1");
    assertFalse(range3.equals(iterationRange1));
    assertFalse(range3.equals(iterationRange2));

    ClawRange range4 = new ClawRange("i", "2", "10", "1");
    assertFalse(range4.equals(iterationRange1));
    assertFalse(range4.equals(iterationRange2));

    ClawRange range5 = new ClawRange("j", "1", "10", "1");
    assertFalse(range5.equals(iterationRange1));
    assertFalse(range5.equals(iterationRange2));

    ClawRange range6 = new ClawRange("i", "istart", "iend", "1");
    assertTrue(range6.equals(iterationRange2));
    assertFalse(range6.equals(iterationRange1));

    ClawRange range7 = new ClawRange("i", "istart", "iend", "2");
    assertFalse(range7.equals(iterationRange2));
    assertFalse(range7.equals(iterationRange1));

    ClawRange range8 = new ClawRange("i", "istart", "ieend", "1");
    assertFalse(range8.equals(iterationRange2));
    assertFalse(range8.equals(iterationRange1));

    ClawRange range9 = new ClawRange("i", "istarter", "iend", "1");
    assertFalse(range9.equals(iterationRange2));
    assertFalse(range9.equals(iterationRange1));

    ClawRange range10 = new ClawRange("j", "istart", "iend", "1");
    assertFalse(range10.equals(iterationRange2));
    assertFalse(range10.equals(iterationRange1));
  }
}
