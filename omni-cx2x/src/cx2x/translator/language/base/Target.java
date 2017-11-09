/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language.base;

import cx2x.translator.common.ClawConstant;

import java.util.ArrayList;
import java.util.List;

/**
 * Enumeration that define the possible target supported.
 * Currently CPU, GPU and MIC.
 *
 * @author clementval
 */
public enum Target {
  CPU(ClawConstant.TARGET_CPU),
  GPU(ClawConstant.TARGET_GPU),
  MIC(ClawConstant.TARGET_MIC),
  FPGA(ClawConstant.TARGET_FPGA),
  NONE(ClawConstant.TARGET_NONE);

  private final String code;

  Target(String code) {
    this.code = code;
  }

  public static List<String> availableTargets() {
    List<String> codes = new ArrayList<>();
    for(Target t : Target.values()) {
      codes.add(t.code);
    }
    return codes;
  }

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. CPU otherwise.
   */
  public static Target fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case ClawConstant.TARGET_CPU:
        return CPU;
      case ClawConstant.TARGET_GPU:
        return GPU;
      case ClawConstant.TARGET_MIC:
        return MIC;
      case ClawConstant.TARGET_FPGA:
        return FPGA;
      default:
        return NONE;
    }
  }

  /**
   * Convert current enum to String value.
   *
   * @return Corresponding String value.
   */
  @Override
  public String toString() {
    switch(this) {
      case GPU:
        return ClawConstant.TARGET_GPU;
      case CPU:
        return ClawConstant.TARGET_CPU;
      case MIC:
        return ClawConstant.TARGET_MIC;
      case FPGA:
        return ClawConstant.TARGET_FPGA;
      default:
        return ClawConstant.TARGET_NONE;
    }
  }
}
