/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import helper.TestConstant;
import helper.XmlHelper;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static org.junit.Assert.*;

/**
 * Test methods of the Xnode class
 *
 * @author clementval
 */
public class XnodeTest {

  private static final String arg1 = "<FarrayRef type=\"A7f899b41b220\">" +
      "<varRef type=\"A7f899b411d50\">" +
      "<Var scope=\"local\" type=\"A7f899b411d50\">q</Var></varRef>" +
      "<indexRange is_assumed_shape=\"true\"/><indexRange>" +
      "<lowerBound><FintConstant type=\"Fint\">1</FintConstant></lowerBound>" +
      "<upperBound><FintConstant type=\"Fint\">60</FintConstant></upperBound>" +
      "</indexRange></FarrayRef>";
  private static final String arg2 = "<FarrayRef type=\"A7f899b41b220\">" +
      "<varRef type=\"A7f899b411d50\">" +
      "<Var scope=\"local\" type=\"A7f899b411d50\">p</Var></varRef>" +
      "<indexRange is_assumed_shape=\"true\"/>" +
      "<indexRange is_assumed_shape=\"true\"/></FarrayRef>";
  private static final String arg3 = "<FarrayRef type=\"A7fef50d1c8a0\">" +
      "<varRef type=\"A7fef50d1bc20\">" +
      "<FmemberRef type=\"A7fef50d1bc20\" member=\"y\">" +
      "<varRef type=\"S7fef50d11940\">" +
      "<Var type=\"S7fef50d11940\" scope=\"local\">ty</Var></varRef>" +
      "</FmemberRef></varRef>" +
      "<arrayIndex><Var type=\"Fint\" scope=\"local\">p</Var></arrayIndex>" +
      "<indexRange is_assumed_shape=\"true\"></indexRange></FarrayRef>";
  private static final String arg4 = "<namedValue name=\"nproma\">" +
      "<Var scope=\"local\" type=\"Fint\">nproma</Var>" +
      "</namedValue>";
  private static final String arg5 = "<FmemberRef member=\"end\" " +
      "type=\"I7f87cdc65f00\"><varRef type=\"S7f87cdca1c30\">" +
      "<FmemberRef member=\"middle\" type=\"S7f87cdca1c30\">" +
      "<varRef type=\"S7f87cdc99e40\">" +
      "<Var scope=\"local\" type=\"S7f87cdc99e40\">first</Var>" +
      "</varRef></FmemberRef></varRef></FmemberRef>";
  private static final String arg6 = "<FarrayRef type=\"A7fef50d1c8a0\">" +
      "<varRef type=\"A7fef50d1bc20\">" +
      "<FmemberRef member=\"end\" type=\"I7f87cdc65f00\">" +
      "<varRef type=\"S7f87cdca1c30\">" +
      "<FmemberRef member=\"middle\" type=\"S7f87cdca1c30\">" +
      "<varRef type=\"S7f87cdc99e40\">" +
      "<Var scope=\"local\" type=\"S7f87cdc99e40\">first</Var>" +
      "</varRef></FmemberRef></varRef></FmemberRef></varRef><arrayIndex>" +
      "<Var type=\"Fint\" scope=\"local\">p</Var></arrayIndex>" +
      "<indexRange is_assumed_shape=\"true\"></indexRange></FarrayRef>";
  private static final String arg7 = "<FarrayRef type=\"A7fb5db2d0b80\">" +
      "<varRef type=\"A7fb5db2cfee0\">" +
      "<FmemberRef member=\"t\" type=\"A7fb5db2cfee0\">" +
      "<varRef type=\"S7fb5db321970\">" +
      "<FarrayRef type=\"S7fb5db321970\"><varRef type=\"A7fb5db321a90\">" +
      "<Var scope=\"local\" type=\"A7fb5db321a90\">tend</Var>" +
      "</varRef>" +
      "<arrayIndex><Var scope=\"local\" type=\"I7fb5db1c0430\">blockid</Var>" +
      "</arrayIndex></FarrayRef></varRef></FmemberRef></varRef><arrayIndex>" +
      "<Var scope=\"local\" type=\"I7fb5db1c1800\">jl</Var></arrayIndex>" +
      "<indexRange is_assumed_shape=\"true\"></indexRange></FarrayRef>";
  private static final String arg8 = "<FarrayRef type=\"A7fb5db2e6200\">" +
      "<varRef type=\"A7fb5db2e4e60\">" +
      "<FmemberRef member=\"t\" type=\"A7fb5db2e4e60\">" +
      "<varRef type=\"S7fb5db322d10\">" +
      "<FarrayRef type=\"S7fb5db322d10\"><varRef type=\"A7fb5db322e30\">" +
      "<Var scope=\"local\" type=\"A7fb5db322e30\">tend</Var></varRef>" +
      "<arrayIndex>" +
      "<Var scope=\"local\" type=\"I7fb5db1c0430\">blockid</Var>" +
      "</arrayIndex></FarrayRef></varRef></FmemberRef></varRef>" +
      "<arrayIndex><Var scope=\"local\" type=\"I7fb5db1c1800\">jl</Var>" +
      "</arrayIndex><indexRange is_assumed_shape=\"true\"></indexRange>" +
      "<indexRange is_assumed_shape=\"true\"></indexRange></FarrayRef>";
  private static final String arg9 = "<FarrayRef type=\"A7f899b41b220\">" +
      "<varRef type=\"A7f899b411d50\">" +
      "<Var scope=\"local\" type=\"A7f899b411d50\">q</Var></varRef>" +
      "<indexRange is_assumed_shape=\"true\"/></FarrayRef>";
  private static final String arg10 = "<FarrayRef type=\"A7f899b41b220\">" +
      "<varRef type=\"A7f899b411d50\">" +
      "<Var scope=\"local\" type=\"A7f899b411d50\">q</Var></varRef>" +
      "</FarrayRef>";

  @Test
  public void constructStringTest() {
    Xnode arg1Node = XmlHelper.createXnode(arg1);
    assertNotNull(arg1Node);
    assertEquals("q(:,1:60)",
        arg1Node.constructRepresentation(false));

    Xnode arg2Node = XmlHelper.createXnode(arg2);
    assertNotNull(arg2Node);
    assertEquals("p(:,:)", arg2Node.constructRepresentation(false));

    Xnode arg3Node = XmlHelper.createXnode(arg3);
    assertNotNull(arg3Node);
    assertEquals("ty%y(p,:)", arg3Node.constructRepresentation(false));

    Xnode arg4Node = XmlHelper.createXnode(arg4);
    assertNotNull(arg4);
    assertEquals("nproma=nproma", arg4Node.constructRepresentation(true));
    assertEquals("nproma", arg4Node.constructRepresentation(false));

    Xnode arg5Node = XmlHelper.createXnode(arg5);
    assertNotNull(arg5Node);
    assertEquals("first%middle%end", arg5Node.constructRepresentation(false));

    Xnode arg6Node = XmlHelper.createXnode(arg6);
    assertNotNull(arg6Node);
    assertEquals("first%middle%end(p,:)",
        arg6Node.constructRepresentation(false));

    Xnode arg7Node = XmlHelper.createXnode(arg7);
    assertNotNull(arg7Node);
    assertEquals("tend(blockid)%t(jl,:)",
        arg7Node.constructRepresentation(false));

    Xnode arg8Node = XmlHelper.createXnode(arg8);
    assertNotNull(arg8Node);
    assertEquals("tend(blockid)%t(jl,:,:)",
        arg8Node.constructRepresentation(false));

    Xnode arg9Node = XmlHelper.createXnode(arg9);
    assertNotNull(arg9Node);
    assertEquals("q(:)", arg9Node.constructRepresentation(false));

    Xnode arg10Node = XmlHelper.createXnode(arg10);
    assertNotNull(arg10Node);
    assertEquals("q", arg10Node.constructRepresentation(false));

    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode n1 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    assertEquals("", n1.constructRepresentation(false));

    Xnode n2 = xcodeml.createNode(Xcode.F_DO_STATEMENT);
    assertEquals("", n2.constructRepresentation(false));
  }

  @Test
  public void utilityMethodTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    assertNotNull(xcodeml);
    List<Xnode> pragmas = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
    assertTrue(pragmas.size() > 0);
    Xnode p = pragmas.get(0);

    assertNotNull(p.findParentFunction());
    assertNull(p.findParentFunction().findParentFunction());

    Xnode p2 = xcodeml.createNode(Xcode.F_PRAGMA_STATEMENT);
    p.copyAttribute(p2, Xattr.LINENO);
    assertEquals("FpragmaStatement (children: 0)", p2.toString());

    // No first child
    assertFalse(p.compareFirstChildValues(null));
    assertFalse(p.compareFirstChildValues(p2));

    // Depth
    assertTrue(p.depth() > 0);

    // Test attributes query and set
    assertNull(p2.child(10));
    p2.setBooleanAttribute(Xattr.IS_ASSUMED_SHAPE, false);
    assertFalse(p2.getBooleanAttribute(Xattr.IS_ASSUMED_SHAPE));
    p2.removeAttribute(Xattr.IS_ASSUMED_SHAPE);
    assertFalse(p2.hasAttribute(Xattr.IS_ASSUMED_SHAPE));

    // Append/insert
    Xnode intConst = xcodeml.createIntConstant(10);
    FfunctionDefinition fctDef = p.findParentFunction();
    Xnode clone = intConst.cloneNode();
    assertNotNull(clone);
    assertNotEquals(clone.element(), intConst.element());

    fctDef.body().append(intConst);
    fctDef.body().append(clone);
    assertTrue(intConst.isDirectSibling(clone, Collections.<Xcode>emptyList()));

    fctDef.body().append(null);
    fctDef.body().insert(intConst, true);
    fctDef.body().insert(intConst);
    fctDef.body().insert(null);

    Xnode crt = intConst;
    while(crt != null) {
      assertNotNull(crt);
      crt = crt.prevSibling();
    }

    assertNotNull(fctDef.body().matchDirectDescendant(Xcode.F_PRAGMA_STATEMENT));
    assertNull(p.matchDirectDescendant(Xcode.F_PRAGMA_STATEMENT));

    // Methods should not crash on deleted node
    p.delete();
    assertTrue(p.depth() < 0);
    p.insertAfter(p2);
    p.insertBefore(p2);
    p.matchDescendant(Xcode.F_DO_STATEMENT);
    p.matchDirectDescendant(Xcode.F_DO_STATEMENT);
    p.matchDirectDescendant(Arrays.asList(Xcode.F_DO_STATEMENT,
        Xcode.F_DO_WHILE_STATEMENT));
    assertNull(p.nextSibling());
    assertNull(p.prevSibling());
    assertNull(p.matchSibling(Xcode.F_DO_STATEMENT));
    assertTrue(p.matchAll(Xcode.F_PRAGMA_STATEMENT).size() == 0);
    assertFalse(p.isDirectSibling(null, Collections.<Xcode>emptyList()));
  }

  @Test
  public void hasBodyTest() {
    List<Xcode> expectedNodeWithBody = new ArrayList<>(Arrays.asList(
        Xcode.ASSOCIATE_STATEMENT,
        Xcode.BLOCK_STATEMENT,
        Xcode.CRITICAL_STATEMENT,
        Xcode.ELSE,
        Xcode.F_CASE_LABEL,
        Xcode.F_DO_CONCURRENT_STATEMENT,
        Xcode.F_DO_STATEMENT,
        Xcode.F_DO_WHILE_STATEMENT,
        Xcode.F_FUNCTION_DEFINITION,
        Xcode.FOR_ALL_STATEMENT,
        Xcode.THEN,
        Xcode.TYPE_GUARD
    ));

    for(Xcode x : expectedNodeWithBody) {
      assertTrue(x.hasBody());
    }

    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    for(Xcode opcode : Xcode.values()) {
      Xnode n = xcodeml.createNode(opcode);
      if(expectedNodeWithBody.contains(n.opcode())) {
        assertTrue(n.opcode().hasBody());
        assertTrue(n.hasBody());
      } else {
        assertFalse(n.opcode().hasBody());
        assertFalse(n.hasBody());
      }
    }
  }

  @Test
  public void deletedTest() {
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode n1 = xcodeml.createNode(Xcode.F_BASIC_TYPE);
    assertFalse(n1.isDeleted());
    n1.delete();
    assertTrue(n1.isDeleted());
  }

  @Test
  public void enhancedInfoTest() {
    String filename = "dummy.f90";
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode n1 = xcodeml.createNode(Xcode.VAR_DECL);
    n1.setLine(1);
    n1.setFilename(filename);
    assertEquals(1, n1.lineNo());
    assertEquals(filename, n1.filename());

    Xnode n2 = xcodeml.createNode(Xcode.VAR_DECL);
    n1.copyEnhancedInfo(n2);
    assertEquals(1, n2.lineNo());
    assertEquals(filename, n2.filename());
  }

  @Test
  public void matchAllAncestorTest() {
    XcodeProgram xcodeml =
        XcodeProgram.createFromFile(TestConstant.TEST_ASSIGN_STMT2);
    assertNotNull(xcodeml);

    List<Xnode> nodes = xcodeml.matchAll(Xcode.F_ASSIGN_STATEMENT);
    assertEquals(4, nodes.size());

    List<Xnode> matches1 = nodes.get(0).matchAllAncestor(Xcode.F_IF_STATEMENT,
        Xcode.F_FUNCTION_DEFINITION);
    List<Xnode> matches2 = nodes.get(0).matchAllAncestor(Xcode.F_IF_STATEMENT);
    assertEquals(2, matches1.size());
    assertEquals(2, matches2.size());

    matches1 = nodes.get(1).matchAllAncestor(Xcode.F_IF_STATEMENT,
        Xcode.F_FUNCTION_DEFINITION);
    matches2 = nodes.get(1).matchAllAncestor(Xcode.F_IF_STATEMENT);
    assertEquals(2, matches1.size());
    assertEquals(2, matches2.size());

    matches1 = nodes.get(3).matchAllAncestor(Xcode.F_IF_STATEMENT,
        Xcode.F_FUNCTION_DEFINITION);
    matches2 = nodes.get(3).matchAllAncestor(Xcode.F_IF_STATEMENT);
    assertEquals(0, matches1.size());
    assertEquals(0, matches2.size());
  }
}
