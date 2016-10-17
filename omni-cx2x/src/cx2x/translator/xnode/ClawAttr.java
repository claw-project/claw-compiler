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

  public String toString() {
    return this.name;
  }

  public static ClawAttr fromString(String value) {
    return ClawAttr.valueOf(value.toUpperCase().replace("-", "_"));
  }
}

