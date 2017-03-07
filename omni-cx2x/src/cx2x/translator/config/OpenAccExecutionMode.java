/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.config;

/**
 * Define the possible execution mode for OpenACC.
 *
 * @author clementval
 */
public enum OpenAccExecutionMode {
  NONE,
  VECTOR,
  VECTOR_GANG,
  GANG_VECTOR;
  // TODO refine it

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. VECTOR otherwise.
   */
  public static OpenAccExecutionMode fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case OpenAccConfiguration.EXEC_MODE_VECTOR:
        return VECTOR;
      case OpenAccConfiguration.EXEC_MODE_VECTOR_GANG:
        return VECTOR_GANG;
      case OpenAccConfiguration.EXEC_MODE_GANG_VECTOR:
        return GANG_VECTOR;
      default:
        return NONE;
    }
  }
}
