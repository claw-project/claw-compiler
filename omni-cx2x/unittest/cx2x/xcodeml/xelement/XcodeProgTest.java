/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import java.io.File;
import static org.junit.Assert.*;
import org.junit.Test;

/**
 * Test the features of the XcodeProgram class
 *
 * @author clementval
 */

public class XcodeProgTest {

  // Path is relative to the test directory
  private static final String TEST_DATA = "./data/basic.xcml";

  @Test
  public void basicXcodeProgTest() {
    File f = new File(TEST_DATA);
    System.out.println("Working Directory = " +
        System.getProperty("user.dir"));
    assertTrue(f.exists());
    XcodeProgram xcodeml = new XcodeProgram(TEST_DATA);
    boolean loaded = xcodeml.load();
    assertTrue(loaded);
    assertEquals(8, xcodeml.getTypeTable().count());
    assertEquals(2, xcodeml.getGlobalSymbolsTable().count());
  }

}
