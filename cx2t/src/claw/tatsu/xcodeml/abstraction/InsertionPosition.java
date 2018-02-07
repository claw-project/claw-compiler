/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.TatsuConstant;

import java.util.HashMap;
import java.util.Map;

/**
 * Describe the different insertion position used for field promotion.
 *
 * @author clementval
 */
public enum InsertionPosition {
  BEFORE(TatsuConstant.BEFORE),
  IN_MIDDLE(TatsuConstant.MIDDLE),
  AFTER(TatsuConstant.AFTER);

  private static final Map<String, InsertionPosition> _stringToEnum =
      new HashMap<>();

  static {
    for(InsertionPosition position : values()) {
      _stringToEnum.put(position.toString().toLowerCase(), position);
    }
  }

  private final String _value;

  InsertionPosition(String value) {
    _value = value;
  }

  /**
   * Get enum value from String representation.
   *
   * @param value String value to evaluate.
   * @return Enum value matching the String representation. If no match, BEFORE
   * is returned.
   */
  public static InsertionPosition fromString(String value) {
    return (value == null || !_stringToEnum.containsKey(value.toLowerCase())) ?
        BEFORE : _stringToEnum.get(value.toLowerCase());
  }

  @Override
  public String toString() {
    return _value;
  }
}
