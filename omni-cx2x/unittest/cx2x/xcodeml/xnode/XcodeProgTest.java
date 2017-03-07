/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import helper.TestConstant;
import org.junit.Test;

import java.io.File;

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
    assertEquals(8, xcodeml.getTypeTable().count());
    assertEquals(2, xcodeml.getGlobalSymbolsTable().count());
    assertEquals(2, xcodeml.getGlobalDeclarationsTable().count());
  }

}
