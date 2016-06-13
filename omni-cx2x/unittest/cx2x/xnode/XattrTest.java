/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xnode;

import cx2x.xcodeml.xnode.Xattr;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author clementval
 */
public class XattrTest {

  @Test
  public void stringToEnumTest(){
    assertEnum(Xattr.COMPILER_INFO,"compiler-info");
    assertEnum(Xattr.CONSTRUCT_NAME,"construct_name");
    assertEnum(Xattr.FILE,"file");
    assertEnum(Xattr.INTENT,"intent");
    assertEnum(Xattr.IS_ASSUMED_SHAPE,"is_assumed_shape");
    assertEnum(Xattr.IS_ALLOCATABLE,"is_allocatable");
    assertEnum(Xattr.IS_EXTERNAL,"is_external");
    assertEnum(Xattr.IS_INTERNAL,"is_internal");
    assertEnum(Xattr.IS_INTRINSIC,"is_intrinsic");
    assertEnum(Xattr.IS_OPTIONAL,"is_optional");
    assertEnum(Xattr.IS_PARAMETER,"is_parameter");
    assertEnum(Xattr.IS_POINTER,"is_pointer");
    assertEnum(Xattr.IS_PRIVATE,"is_private");
    assertEnum(Xattr.IS_PROGRAM,"is_program");
    assertEnum(Xattr.IS_PUBLIC,"is_public");
    assertEnum(Xattr.IS_RECURSIVE,"is_recursive");
    assertEnum(Xattr.IS_SAVE,"is_save");
    assertEnum(Xattr.IS_TARGET,"is_target");
    assertEnum(Xattr.KIND,"kind");
    assertEnum(Xattr.LANGUAGE,"language");
    assertEnum(Xattr.LINENO,"lineno");
    assertEnum(Xattr.NAME,"name");
    assertEnum(Xattr.REPEAT_COUNT,"repeat_count");
    assertEnum(Xattr.REF,"ref");
    assertEnum(Xattr.RESULT_NAME,"result_name");
    assertEnum(Xattr.RETURN_TYPE,"return_type");
    assertEnum(Xattr.SCLASS,"sclass");
    assertEnum(Xattr.SCOPE,"scope");
    assertEnum(Xattr.SOURCE,"source");
    assertEnum(Xattr.TIME,"time");
    assertEnum(Xattr.TYPE,"type");
    assertEnum(Xattr.VERSION,"version");
  }

  private void assertEnum(Xattr opcode, String value){
    Xattr ret = Xattr.fromString(value);
    assertEquals(opcode, ret);
  }
}
