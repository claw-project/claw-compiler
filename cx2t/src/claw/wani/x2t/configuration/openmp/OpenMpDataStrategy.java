/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.openmp;

/**
 * Define the enum values for the data strategy for OpenMP
 *
 * @author peclatj
 */
public enum OpenMpDataStrategy {
  NONE,
  PRESENT,
  KERNEL;

  /**
   * Get enum value from configuration string.
   *
   * @param value String value from configuration.
   * @return Enum value corresponding to the string. Default is NONE.
   */
  public static OpenMpDataStrategy fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case OpenMpConfiguration.DATA_STRATEGY_KERNEL:
        return KERNEL;
      case OpenMpConfiguration.DATA_STRATEGY_PRESENT:
        return PRESENT;
      case OpenMpConfiguration.DATA_STRATEGY_NONE:
        return NONE;
      default:
        return NONE;
    }
  }
}
