/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import helper.XmlHelper;
import org.junit.Test;

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
    Xnode n1 = xcodeml.createNode(Xcode.FPRAGMASTATEMENT);
    assertEquals("", n1.constructRepresentation(false));

    Xnode n2 = xcodeml.createNode(Xcode.FDOSTATEMENT);
    assertEquals("", n2.constructRepresentation(false));
  }

  @Test
  public void hasBodyTest(){
    XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram();
    Xnode n1 = xcodeml.createNode(Xcode.ASSOCIATESTATEMENT);
    Xnode n2 = xcodeml.createNode(Xcode.BLOCKSTATEMENT);
    Xnode n3 = xcodeml.createNode(Xcode.CRITICALSTATEMENT);
    Xnode n4 = xcodeml.createNode(Xcode.ELSE);
    Xnode n5 = xcodeml.createNode(Xcode.FCASELABEL);
    Xnode n6 = xcodeml.createNode(Xcode.FDOCONCURRENTSTATEMENT);
    Xnode n7 = xcodeml.createNode(Xcode.FDOSTATEMENT);
    Xnode n8 = xcodeml.createNode(Xcode.FDOWHILESTATEMENT);
    Xnode n9 = xcodeml.createNode(Xcode.FFUNCTIONDEFINITION);
    Xnode n10 = xcodeml.createNode(Xcode.FORALLSTATEMENT);
    Xnode n11 = xcodeml.createNode(Xcode.THEN);
    Xnode n12 = xcodeml.createNode(Xcode.TYPEGUARD);
    Xnode n13 = xcodeml.createNode(Xcode.FBASICTYPE);

    assertTrue(n1.hasBody());
    assertTrue(n2.hasBody());
    assertTrue(n3.hasBody());
    assertTrue(n4.hasBody());
    assertTrue(n5.hasBody());
    assertTrue(n6.hasBody());
    assertTrue(n7.hasBody());
    assertTrue(n8.hasBody());
    assertTrue(n9.hasBody());
    assertTrue(n10.hasBody());
    assertTrue(n11.hasBody());
    assertTrue(n12.hasBody());
    assertFalse(n13.hasBody());
  }
}
