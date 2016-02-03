/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import static org.junit.Assert.*;
import org.junit.Test;
import helper.XmlHelper;

/**
 * Test features of the XdoStatement class.
 * @author clementval
 */
public class XdoStatementTest {

  private static final String simpleDoStatement =
      "<FdoStatement lineno=\"10\" file=\"original_code.f90\">" +
      "<Var type=\"Fint\" scope=\"local\">i</Var>" +
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
      "</indexRange>" +
      "<body>" +
      "</body>" +
      "</FdoStatement>";

  @Test
  public void simpleDoStatementTest(){
    XdoStatement doStmt = XmlHelper.createXdoStatement(simpleDoStatement);
    assertNotNull(doStmt);
    assertEquals(10, doStmt.getLineNo());
    assertEquals("original_code.f90", doStmt.getFile());
    assertNotNull(doStmt.getIterationRange());
    assertNotNull(doStmt.getIterationRange().getInductionVar());
    assertNotNull(doStmt.getIterationRange().getIndexRange());
    assertNotNull(doStmt.getIterationRange().getIndexRange().getLowerBound());
    assertNotNull(doStmt.getIterationRange().getIndexRange().getUpperBound());
    assertNotNull(doStmt.getIterationRange().getIndexRange().getStep());

    assertEquals("i", doStmt.getIterationRange().getInductionVar().getValue());
    assertEquals("1",
        doStmt.getIterationRange().getIndexRange().getLowerBound().getValue());
    assertEquals("10",
        doStmt.getIterationRange().getIndexRange().getUpperBound().getValue());
    assertEquals("1",
        doStmt.getIterationRange().getIndexRange().getStep().getValue());
    assertNotNull(doStmt.getBody());

  }

}
