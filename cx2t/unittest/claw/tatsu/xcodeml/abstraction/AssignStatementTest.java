/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.primitive.Function;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import helper.TestConstant;
import org.junit.Test;

import java.util.List;
import java.util.Set;

import static junit.framework.TestCase.*;

/**
 * Test features of the AssignStatement class.
 *
 * @author clementval
 */
public class AssignStatementTest {

  @Test
  public void gatherAssignmentTest1() {
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT);
    assertNotNull(xcodeml);

    List<Xnode> nodes = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
    assertEquals(1, nodes.size());

    assertEquals(Xcode.F_FUNCTION_DEFINITION, nodes.get(0).opcode());
    FfunctionDefinition fctDef = new FfunctionDefinition(nodes.get(0));

    List<AssignStatement> assignStatements =
        Function.gatherAssignStatements(fctDef);
    assertEquals(2, assignStatements.size());

    assertTrue(assignStatements.get(0).isChildOf(Xcode.F_IF_STATEMENT));
    assertFalse(assignStatements.get(1).isChildOf(Xcode.F_IF_STATEMENT));

    Set<String> vars = assignStatements.get(0).getVarRefNames();
    assertEquals(2, vars.size());
    assertTrue(vars.contains("t"));
    assertTrue(vars.contains("q"));
  }

  @Test
  public void gatherAssignmentTest2() {
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT2);
    assertNotNull(xcodeml);

    List<Xnode> nodes = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
    assertEquals(1, nodes.size());

    assertEquals(Xcode.F_FUNCTION_DEFINITION, nodes.get(0).opcode());
    FfunctionDefinition fctDef = new FfunctionDefinition(nodes.get(0));

    List<AssignStatement> assignStatements =
        Function.gatherAssignStatements(fctDef);
    assertEquals(4, assignStatements.size());
  }
}
