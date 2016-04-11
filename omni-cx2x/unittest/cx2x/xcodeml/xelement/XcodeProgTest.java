/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import java.io.File;
import static org.junit.Assert.*;

import helper.XmlHelper;
import org.junit.Test;

/**
 * Test the features of the XcodeProgram class
 *
 * @author clementval
 */

public class XcodeProgTest {



  @Test
  public void basicXcodeProgTest() {
    File f = new File(XmlHelper.TEST_DATA);
    assertTrue(f.exists());
    XcodeProgram xcodeml = new XcodeProgram(XmlHelper.TEST_DATA);
    boolean loaded = xcodeml.load();
    assertTrue(loaded);
    assertEquals(8, xcodeml.getTypeTable().count());
    assertEquals(2, xcodeml.getGlobalSymbolsTable().count());
  }

}
