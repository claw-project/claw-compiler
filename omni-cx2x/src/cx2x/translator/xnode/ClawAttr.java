/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.xnode;

import cx2x.translator.common.ClawConstant;

/**
 * CLAW element specialized attributes code.
 *
 * @author clementval
 */
public enum ClawAttr {
  IS_CLAW(ClawConstant.IS_CLAW),
  OVER(ClawConstant.OVER);

  private final String name;

  ClawAttr(String s) {
    name = s;
  }

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. Null otherwise.
   */
  public static ClawAttr fromString(String value) {
    if(value == null) {
      return null;
    }
    switch(value) {
      case ClawConstant.IS_CLAW:
        return IS_CLAW;
      case ClawConstant.OVER:
        return OVER;
    }
    return null;
  }

  public String toString() {
    return this.name;
  }
}

