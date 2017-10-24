/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.xnode;

import java.util.Random;

/**
 * Enum representing the type coming from OMNI Compiler.
 *
 * @author clementval
 */
public enum XcodeType {

  // Type prefix from OMNI Compiler. Taken for the F-output-xcodeml.c file.
  ARRAY('A'),
  CHARACTER('C'),
  COMPLEX('P'),
  DCOMPLEX('P'),
  DREAL('R'),
  ENUM('E'),
  FUNCTION('F'),
  GENERIC('V'),
  GNUMERIC('U'),
  GNUMERIC_ALL('V'),
  INTEGER('I'),
  LHS('V'),
  LOGICAL('L'),
  NAMELIST('N'),
  REAL('R'),
  STRUCT('S'),
  SUBROUTINE('F');

  private static final int HASH_LENGTH = 12;
  private final char _prefix;

  XcodeType(char prefix) {
    _prefix = prefix;
  }

  /**
   * Get a new unique hash with the current XcodeType prefix.
   *
   * @return New unique hash.
   */
  public String generateHash() {
    return _prefix + generateHash(HASH_LENGTH);
  }

  /**
   * Generate a new unique type hash for the table.
   *
   * @param length Length of the hash string to be generated.
   * @return The new unique hash.
   */
  private String generateHash(int length) {
    Random r = new Random();
    StringBuilder sb = new StringBuilder();
    while(sb.length() < length) {
      sb.append(Integer.toHexString(r.nextInt()));
    }
    return sb.toString().substring(0, length);
  }

  /**
   * Check whether the giving hash is from the current type.
   *
   * @param hash Hash to be checked.
   * @return True if the hash is of current type. False otherwise.
   */
  public boolean isOfType(String hash) {
    return hash != null && !hash.isEmpty() && hash.charAt(0) == _prefix;
  }

  /**
   * Check whether the given type is a built-in type or is a type defined in the
   * type table.
   *
   * @param type Type to check.
   * @return True if the type is built-in. False otherwise.
   */
  public static boolean isBuiltInType(String type) {
    if(type == null) {
      return false;
    }
    switch(type) {
      case Xname.TYPE_F_CHAR:
      case Xname.TYPE_F_COMPLEX:
      case Xname.TYPE_F_INT:
      case Xname.TYPE_F_LOGICAL:
      case Xname.TYPE_F_REAL:
      case Xname.TYPE_F_VOID:
        return true;
      default:
        return false;
    }
  }

}
