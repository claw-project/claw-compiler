/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language.language;

import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawRange;
import helper.XmlHelper;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 * Test the features of the ClawRange class
 *
 * @author clementval
 */
public class ClawRangeTest {

  private static final String beginLoop = "<FdoStatement>";
  private static final String endLoop = "<body></body></FdoStatement>";
  private static final String inductionVar1 =
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
  public void compareWithLoopIterationRangeTest() {
    Xnode iterationRange1 = XmlHelper.createXnode(beginLoop +
        inductionVar1 + indexRange1 + endLoop);
    assertNotNull(iterationRange1);
    Xnode iterationRange2 = XmlHelper.createXnode(beginLoop +
        inductionVar1 + indexRange2 + endLoop);
    assertNotNull(iterationRange1);

    ClawRange range1 = new ClawRange("i", "1", "10", "1");
    assertTrue(range1.compareToDoStmt(iterationRange1));
    assertFalse(range1.compareToDoStmt(iterationRange2));

    ClawRange range2 = new ClawRange("i", "1", "10", "2");
    assertFalse(range2.compareToDoStmt(iterationRange1));
    assertFalse(range2.compareToDoStmt(iterationRange2));

    ClawRange range3 = new ClawRange("i", "1", "11", "1");
    assertFalse(range3.compareToDoStmt(iterationRange1));
    assertFalse(range3.compareToDoStmt(iterationRange2));

    ClawRange range4 = new ClawRange("i", "2", "10", "1");
    assertFalse(range4.compareToDoStmt(iterationRange1));
    assertFalse(range4.compareToDoStmt(iterationRange2));

    ClawRange range5 = new ClawRange("j", "1", "10", "1");
    assertFalse(range5.compareToDoStmt(iterationRange1));
    assertFalse(range5.compareToDoStmt(iterationRange2));

    ClawRange range6 = new ClawRange("i", "istart", "iend", "1");
    assertTrue(range6.compareToDoStmt(iterationRange2));
    assertFalse(range6.compareToDoStmt(iterationRange1));

    ClawRange range7 = new ClawRange("i", "istart", "iend", "2");
    assertFalse(range7.compareToDoStmt(iterationRange2));
    assertFalse(range7.compareToDoStmt(iterationRange1));

    ClawRange range8 = new ClawRange("i", "istart", "ieend", "1");
    assertFalse(range8.compareToDoStmt(iterationRange2));
    assertFalse(range8.compareToDoStmt(iterationRange1));

    ClawRange range9 = new ClawRange("i", "istarter", "iend", "1");
    assertFalse(range9.compareToDoStmt(iterationRange2));
    assertFalse(range9.compareToDoStmt(iterationRange1));

    ClawRange range10 = new ClawRange("j", "istart", "iend", "1");
    assertFalse(range10.compareToDoStmt(iterationRange2));
    assertFalse(range10.compareToDoStmt(iterationRange1));

    ClawRange range11 = new ClawRange();
    range11.setInductionVar("i");
    range11.setLowerBound("istart");
    range11.setUpperBound("iend");
    assertTrue(range11.compareToDoStmt(iterationRange2));

  }
}
