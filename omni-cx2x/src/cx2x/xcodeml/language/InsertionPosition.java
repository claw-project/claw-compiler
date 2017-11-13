/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.xcodeml.language;

import cx2x.translator.common.ClawConstant;

/**
 * Describe the different insertion position used for field promotion.
 *
 * @author clementval
 */
public enum InsertionPosition {
  BEFORE,
  IN_MIDDLE,
  AFTER;

  public static InsertionPosition fromString(String value) {
    if(value == null) {
      return BEFORE;
    }
    switch(value.toLowerCase()) {
      case ClawConstant.BEFORE:
        return BEFORE;
      case ClawConstant.MIDDLE:
        return IN_MIDDLE;
      case ClawConstant.AFTER:
        return AFTER;
      default:
        return BEFORE;
    }
  }

  @Override
  public String toString() {
    switch(this) {
      case IN_MIDDLE:
        return ClawConstant.MIDDLE;
      case AFTER:
        return ClawConstant.AFTER;
      default:
        return ClawConstant.BEFORE;
    }
  }
}
