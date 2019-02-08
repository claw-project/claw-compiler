/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import java.util.HashMap;
import java.util.Map;

/**
 * Intrinsic function name as enumeration.
 *
 * @author clementval
 */
public enum Xintrinsic {
  PRESENT;

  private static final Map<String, Xintrinsic> _stringToEnum = new HashMap<>();

  static {
    for(Xintrinsic value : values()) {
      _stringToEnum.put(value.toString().toLowerCase(), value);
    }
  }

  public static Xintrinsic fromString(String value) {
    return (value == null || !_stringToEnum.containsKey(value.toLowerCase())) ?
        null : _stringToEnum.get(value.toLowerCase());
  }
}
