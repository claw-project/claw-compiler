/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.x2t.configuration.openacc;

/**
 * Define the enum values for the data strategy for OpenACC
 *
 * @author clementval
 */
public enum OpenAccDataStrategy {
  NONE,
  PRESENT,
  KERNEL;

  /**
   * Get enum value from configuration string.
   *
   * @param value String value from configuration.
   * @return Enum value corresponding to the string. Default is NONE.
   */
  public static OpenAccDataStrategy fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case OpenAccConfiguration.DATA_STRATEGY_KERNEL:
        return KERNEL;
      case OpenAccConfiguration.DATA_STRATEGY_PRESENT:
        return PRESENT;
      case OpenAccConfiguration.DATA_STRATEGY_NONE:
        return NONE;
      default:
        return NONE;
    }
  }
}
