/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.TestConstant;
import org.junit.Test;

import java.util.Collections;
import java.util.List;

import static junit.framework.TestCase.*;

/**
 * Test features of the condition primitive class.
 *
 * @author clementval
 */
public class ConditionTest {

  @Test
  public void dependsOnTest() {
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT);
    assertNotNull(xcodeml);

    List<Xnode> nodes = xcodeml.matchAll(Xcode.F_IF_STATEMENT);
    assertEquals(1, nodes.size());

    Xnode ifStmt = nodes.get(0);
    assertEquals(Xcode.F_IF_STATEMENT, ifStmt.opcode());

    Xnode condition = ifStmt.matchDirectDescendant(Xcode.CONDITION);
    assertNotNull(condition);

    assertFalse(Condition.dependsOn(condition,
        Collections.<String>emptyList()));
    assertFalse(Condition.dependsOn(null, Collections.singletonList("t")));
    assertFalse(Condition.dependsOn(ifStmt, Collections.singletonList("t")));
    assertTrue(Condition.dependsOn(condition, Collections.singletonList("t")));
    assertFalse(Condition.dependsOn(condition, Collections.singletonList("k")));
  }
}
