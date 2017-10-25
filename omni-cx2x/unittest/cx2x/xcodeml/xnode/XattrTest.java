/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author clementval
 */
public class XattrTest {

  @Test
  public void stringToEnumTest() {
    for(Xattr attrCode : Xattr.values()) {
      String rep = attrCode.toString();
      Xattr attr = Xattr.fromString(rep);
      assertEquals(attrCode, attr);
    }
  }


}
