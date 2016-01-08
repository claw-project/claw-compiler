package xelement;

import java.io.File;
import static org.junit.Assert.*;
import org.junit.Test;
import org.w3c.dom.Element;
import helper.XmlHelper;
import x2x.translator.xcodeml.xelement.XcodeProg;

public class XcodeProgTest {

  // Path is relative to the test directory
  private static final String TEST_DATA = "./data/basic.xcml";

  @Test
  public void basicXcodeProgTest() {
    File f = new File(TEST_DATA);
    assertTrue(f.exists());
    XcodeProg xcodeml = new XcodeProg(TEST_DATA);
    boolean loaded = xcodeml.load();
    assertTrue(loaded);
    assertEquals(8, xcodeml.getTypeTable().count());
    assertEquals(2, xcodeml.getGlobalSymbolsTable().count());
  }

}
