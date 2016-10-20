/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.common;

import cx2x.translator.common.ClawConstant;

/**
 * CLAW over position enumeration. Three possible positions.
 * Before --> over(dimension,:)
 * In middle --> over(:,dimension,:)
 * After --> over(:,dimension)
 *
 * @author clementval
 */
public enum OverPosition {
  BEFORE(ClawConstant.BEFORE),
  MIDDLE(ClawConstant.MIDDLE),
  AFTER(ClawConstant.AFTER);

  private final String code;

  OverPosition(String code) {
    this.code = code;
  }

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. BEFORE otherwise.
   */
  public static OverPosition fromString(String value) {
    if(value == null) {
      return BEFORE;
    }
    switch(value) {
      case ClawConstant.BEFORE:
        return BEFORE;
      case ClawConstant.MIDDLE:
        return MIDDLE;
      case ClawConstant.AFTER:
        return AFTER;
      default:
        return BEFORE;
    }
  }

  public String toString() {
    return this.code;
  }
}
