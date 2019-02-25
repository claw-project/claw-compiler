/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import helper.TestConstant;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.util.Collections;

import static org.junit.Assert.*;

/**
 * Test the features of the XcodeProgram class
 *
 * @author clementval
 */

public class XcodeProgTest {

  @Test
  public void basicXcodeProgTest() {
    File f = new File(TestConstant.TEST_DATA);
    assertTrue(f.exists());
    XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DATA);
    assertNotNull(xcodeml);
    assertNotNull(xcodeml.getTime());
    assertNotNull(xcodeml.getCompilerInfo());
    assertNotNull(xcodeml.getVersion());
    assertNotNull(xcodeml.getLanguage());
    assertNotNull(xcodeml.getSource());
    assertNotNull(xcodeml.getSourceFileOnly());
    assertEquals("original_code.f90", xcodeml.getSourceFileOnly());
    assertEquals(8, xcodeml.getTypeTable().size());
    assertEquals(2, xcodeml.getGlobalSymbolsTable().size());
    assertEquals(2, xcodeml.getGlobalDeclarationsTable().size());

    xcodeml.addError("", 0);
    xcodeml.addError("", null);
    assertTrue(xcodeml.getErrors().isEmpty());

    xcodeml.addError("Error1", 1);
    assertEquals(1, xcodeml.getErrors().size());

    xcodeml.addWarning(null, 0);
    xcodeml.addWarning("", 0);
    xcodeml.addWarning(null, Collections.<Integer>emptyList());
    assertTrue(xcodeml.getWarnings().isEmpty());

    xcodeml.addWarning("New warning 1", 1);
    assertEquals(1, xcodeml.getWarnings().size());
  }

  @Test
  public void creationFailingTest() {
    XcodeProgram xc1 = XcodeProgram.createFromDocument(null);
    assertNull(xc1);

    // Simulate STDIN
    ByteArrayInputStream in =
        new ByteArrayInputStream("<dummy></dummy>".getBytes());
    System.setIn(in);

    XcodeProgram xc2 = XcodeProgram.createFromStdInput();
    assertNull(xc2);
  }
}
