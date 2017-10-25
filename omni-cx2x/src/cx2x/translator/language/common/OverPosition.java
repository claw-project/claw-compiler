/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.common;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.language.DimensionDefinition;

import java.util.List;

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

  /**
   * Get the enum value for the over list value.
   *
   * @param overClause List of values in the over clause.
   * @return Position of the newly inserted dimensions compare the the existing
   * ones.
   */
  public static OverPosition fromList(List<String> overClause) {
    // Default place for insertion of new dimension in promotion.
    if(overClause == null || overClause.size() <= 1) {
      return BEFORE;
    }

    // (:,col) or (col,:)
    if(overClause.size() == 2) {
      return overClause.get(0).equals(DimensionDefinition.BASE_DIM) ?
          AFTER : BEFORE;
    }

    // over(:,col,:)
    if(overClause.get(0).equals(DimensionDefinition.BASE_DIM) &&
        overClause.get(overClause.size() - 1).
            equals(DimensionDefinition.BASE_DIM))
    {
      return MIDDLE;
    } else if(overClause.get(0).equals(DimensionDefinition.BASE_DIM)) {
      return AFTER;
    }
    return OverPosition.BEFORE;
  }

  @Override
  public String toString() {
    return this.code;
  }
}
