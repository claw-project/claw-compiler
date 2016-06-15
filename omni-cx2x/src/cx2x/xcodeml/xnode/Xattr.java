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
  COMPILER_INFO(XelementName.ATTR_COMPILER_INFO),
  CONSTRUCT_NAME(XelementName.ATTR_CONSTRUCT_NAME),
  FILE(XelementName.ATTR_FILE),
  INTENT(XelementName.ATTR_INTENT),
  IS_ASSUMED_SHAPE(XelementName.ATTR_IS_ASSUMED_SHAPE),
  IS_ALLOCATABLE(XelementName.ATTR_IS_ALLOCATABLE),
  IS_EXTERNAL(XelementName.ATTR_IS_EXTERNAL),
  IS_INTERNAL(XelementName.ATTR_IS_INTERNAL),
  IS_INTRINSIC(XelementName.ATTR_IS_INTRINSIC),
  IS_OPTIONAL(XelementName.ATTR_IS_OPTIONAL),
  IS_PARAMETER(XelementName.ATTR_IS_PARAMETER),
  IS_POINTER(XelementName.ATTR_IS_POINTER),
  IS_PRIVATE(XelementName.ATTR_IS_PRIVATE),
  IS_PROGRAM(XelementName.ATTR_IS_PROGRAM),
  IS_PUBLIC(XelementName.ATTR_IS_PUBLIC),
  IS_RECURSIVE(XelementName.ATTR_IS_RECURSIVE),
  IS_SAVE(XelementName.ATTR_IS_SAVE),
  IS_TARGET(XelementName.ATTR_IS_TARGET),
  KIND(XelementName.ATTR_KIND),
  LANGUAGE(XelementName.ATTR_LANGUAGE),
  LINENO(XelementName.ATTR_LINENO),
  NAME(XelementName.ATTR_NAME),
  REPEAT_COUNT(XelementName.ATTR_REPEAT_COUNT),
  REF(XelementName.ATTR_REF),
  RESULT_NAME(XelementName.ATTR_RESULT_NAME),
  RETURN_TYPE(XelementName.ATTR_RETURN_TYPE),
  SCLASS(XelementName.ATTR_SCLASS),
  SCOPE(XelementName.ATTR_SCOPE),
  SOURCE(XelementName.ATTR_SOURCE),
  TIME(XelementName.ATTR_TIME),
  TYPE(XelementName.ATTR_TYPE),
  VERSION(XelementName.ATTR_VERSION);

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
