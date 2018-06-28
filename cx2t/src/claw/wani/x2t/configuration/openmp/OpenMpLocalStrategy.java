/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.openmp;

/**
 * Define the enum values for the local strategy for OpenMP
 *
 * @author peclatj
 */
public enum OpenMpLocalStrategy {
  PRIVATE,
  PROMOTE;

  /**
   * Get enum value from configuration string.
   *
   * @param value String value from configuration.
   * @return Enum value corresponding to the string. Default is PRIVATE.
   */
  public static OpenMpLocalStrategy fromString(String value) {
    if(value == null) {
      return PRIVATE;
    }
    switch(value) {
      case OpenMpConfiguration.LOCAL_STRATEGY_PRIVATE:
        return PRIVATE;
      case OpenMpConfiguration.LOCAL_STRATEGY_PROMOTE:
        return PROMOTE;
      default:
        return PRIVATE;
    }
  }
}
