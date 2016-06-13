/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * XcodeML element attributes code.
 *
 * @author clementval
 */
public enum Xattr {
  COMPILER_INFO("compiler-info"),
  CONSTRUCT_NAME("construct_name"),
  FILE("file"),
  INTENT("intent"),
  IS_ASSUMED_SHAPE("is_assumed_shape"),
  IS_ALLOCATABLE("is_allocatable"),
  IS_EXTERNAL("is_external"),
  IS_INTERNAL("is_internal"),
  IS_INTRINSIC("is_intrinsic"),
  IS_OPTIONAL("is_optional"),
  IS_PARAMETER("is_parameter"),
  IS_POINTER("is_pointer"),
  IS_PRIVATE("is_private"),
  IS_PROGRAM("is_program"),
  IS_PUBLIC("is_public"),
  IS_RECURSIVE("is_recursive"),
  IS_SAVE("is_save"),
  IS_TARGET("is_target"),
  KIND("kind"),
  LANGUAGE("language"),
  LINENO("lineno"),
  NAME("name"),
  REPEAT_COUNT("repeat_count"),
  REF("ref"),
  RESULT_NAME("result_name"),
  RETURN_TYPE("return_type"),
  SCLASS("sclass"),
  SCOPE("scope"),
  SOURCE("source"),
  TIME("time"),
  TYPE("type"),
  VERSION("version");

  private final String name;

  Xattr(String s) {
    name = s;
  }

  public String toString() {
    return this.name;
  }

  public static Xattr fromString(String value){
    return Xattr.valueOf(value.toUpperCase().replace("-", "_"));
  }

}
