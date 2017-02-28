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
  COMPILER_INFO(Xname.ATTR_COMPILER_INFO),
  CONSTRUCT_NAME(Xname.ATTR_CONSTRUCT_NAME),
  DATAREF(Xname.ATTR_DATAREF),
  FILE(Xname.ATTR_FILE),
  INTENT(Xname.ATTR_INTENT),
  IS_ASSUMED_SHAPE(Xname.ATTR_IS_ASSUMED_SHAPE),
  IS_ALLOCATABLE(Xname.ATTR_IS_ALLOCATABLE),
  IS_EXTERNAL(Xname.ATTR_IS_EXTERNAL),
  IS_INTERNAL(Xname.ATTR_IS_INTERNAL),
  IS_INTRINSIC(Xname.ATTR_IS_INTRINSIC),
  IS_OPERATOR(Xname.ATTR_IS_OPERATOR),
  IS_OPTIONAL(Xname.ATTR_IS_OPTIONAL),
  IS_PARAMETER(Xname.ATTR_IS_PARAMETER),
  IS_POINTER(Xname.ATTR_IS_POINTER),
  IS_PRIVATE(Xname.ATTR_IS_PRIVATE),
  IS_PROGRAM(Xname.ATTR_IS_PROGRAM),
  IS_PUBLIC(Xname.ATTR_IS_PUBLIC),
  IS_PURE(Xname.ATTR_IS_PURE),
  IS_RECURSIVE(Xname.ATTR_IS_RECURSIVE),
  IS_SAVE(Xname.ATTR_IS_SAVE),
  IS_TARGET(Xname.ATTR_IS_TARGET),
  KIND(Xname.ATTR_KIND),
  LANGUAGE(Xname.ATTR_LANGUAGE),
  LINENO(Xname.ATTR_LINENO),
  LOCAL_NAME(Xname.ATTR_LOCAL_NAME),
  NAME(Xname.ATTR_NAME),
  REPEAT_COUNT(Xname.ATTR_REPEAT_COUNT),
  REF(Xname.ATTR_REF),
  RESULT_NAME(Xname.ATTR_RESULT_NAME),
  RETURN_TYPE(Xname.ATTR_RETURN_TYPE),
  SCLASS(Xname.ATTR_SCLASS),
  SCOPE(Xname.ATTR_SCOPE),
  SOURCE(Xname.ATTR_SOURCE),
  TIME(Xname.ATTR_TIME),
  TYPE(Xname.ATTR_TYPE),
  USE_NAME(Xname.ATTR_USE_NAME),
  VERSION(Xname.ATTR_VERSION);

  private final String name;

  Xattr(String s) {
    name = s;
  }

  public static Xattr fromString(String value) {
    return Xattr.valueOf(value.toUpperCase().replace("-", "_"));
  }

  public String toString() {
    return this.name;
  }

}
