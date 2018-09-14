/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.XmlHelper;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Test class NestedDoStatement
 *
 * @author clementval
 */
public class NestedDoStatementTest {

  private static final String group1 = "<FdoStatement>" +
      "<body></body>" +
      "</FdoStatement>";
  private static final String group2 = "<FdoStatement>" +
      "<body><FdoStatement><body></body></FdoStatement></body>" +
      "</FdoStatement>";
  private static final String group3 = "<FdoStatement>" +
      "<body>" +
      "<FdoStatement>" +
      "<body>" +
      "<FdoStatement>" +
      "<body></body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>";
  private static final String swapNodes = "<FdoStatement><Var>i</Var>" +
      "<body>" +
      "<FdoStatement><Var>j</Var>" +
      "<body>" +
      "<FdoStatement><Var>k</Var>" +
      "<body></body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>" +
      "</body>" +
      "</FdoStatement>";

  @Test
  public void ctorTest() {
    // One do statement
    Xnode do1 = XmlHelper.createXnode(group1);
    assertNotNull(do1);
    NestedDoStatement ndo1 = new NestedDoStatement(do1);
    assertNotNull(ndo1);
    assertEquals(1, ndo1.size());
    assertEquals(ndo1.getOuterStatement(), ndo1.getInnerStatement());

    // Two do statements
    Xnode do2 = XmlHelper.createXnode(group2);
    assertNotNull(do2);
    NestedDoStatement ndo2 = new NestedDoStatement(do2);
    assertNotNull(ndo2);
    assertEquals(2, ndo2.size());
    assertEquals(ndo2.get(0), ndo2.getOuterStatement());
    assertEquals(ndo2.get(1), ndo2.getInnerStatement());
    assertNotEquals(ndo2.getOuterStatement(), ndo2.getInnerStatement());

    // Two do statements
    Xnode do3 = XmlHelper.createXnode(group3);
    assertNotNull(do3);
    NestedDoStatement ndo3 = new NestedDoStatement(do3);
    assertNotNull(ndo3);
    assertEquals(3, ndo3.size());
    assertEquals(ndo3.get(0), ndo3.getOuterStatement());
    assertEquals(ndo3.get(2), ndo3.getInnerStatement());
    assertNotEquals(ndo3.getOuterStatement(), ndo3.getInnerStatement());

    // Only two do statements in a potential 3 nested group one
    NestedDoStatement ndo3Only2 = new NestedDoStatement(do3, 2);
    assertNotNull(ndo3Only2);
    assertEquals(2, ndo3Only2.size());
    assertEquals(ndo3Only2.get(0), ndo3Only2.getOuterStatement());
    assertEquals(ndo3Only2.get(1), ndo3Only2.getInnerStatement());
    List<Xnode> doStmts = do3.matchAll(Xcode.F_DO_STATEMENT);
    assertEquals(2, doStmts.size());
  }

  @Test
  public void computeSwappingIndicesTest() {
    Xnode swap = XmlHelper.createXnode(swapNodes);
    assertNotNull(swap);
    NestedDoStatement ndostmt = new NestedDoStatement(swap);

    assertEquals(12,
        ndostmt.computeSwappingIndices(Arrays.asList("i", "j", "k")));
    assertEquals(21,
        ndostmt.computeSwappingIndices(Arrays.asList("i", "k", "j")));
    assertEquals(210,
        ndostmt.computeSwappingIndices(Arrays.asList("k", "j", "i")));
    assertEquals(102,
        ndostmt.computeSwappingIndices(Arrays.asList("j", "i", "k")));
    assertEquals(201,
        ndostmt.computeSwappingIndices(Arrays.asList("j", "k", "i")));
    assertEquals(120,
        ndostmt.computeSwappingIndices(Arrays.asList("k", "i", "j")));

    assertEquals(12,
        ndostmt.computeSwappingIndices(Arrays.asList("I", "J", "K")));
    assertEquals(21,
        ndostmt.computeSwappingIndices(Arrays.asList("I", "K", "J")));
    assertEquals(210,
        ndostmt.computeSwappingIndices(Arrays.asList("K", "J", "I")));
    assertEquals(102,
        ndostmt.computeSwappingIndices(Arrays.asList("J", "I", "K")));
    assertEquals(201,
        ndostmt.computeSwappingIndices(Arrays.asList("J", "K", "I")));
    assertEquals(120,
        ndostmt.computeSwappingIndices(Arrays.asList("K", "I", "J")));

    assertEquals(0,
        ndostmt.computeSwappingIndices(Collections.<String>emptyList()));
    assertEquals(0,
        ndostmt.computeSwappingIndices(Arrays.asList("i", "j")));
  }
}
