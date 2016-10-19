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

  private final String name;

  OverPosition(String s) {
    name = s;
  }

  public String toString() {
    return this.name;
  }

  public static OverPosition fromString(String value) {
    return (value == null) ? BEFORE :
        OverPosition.valueOf(value.toUpperCase());
  }
}
