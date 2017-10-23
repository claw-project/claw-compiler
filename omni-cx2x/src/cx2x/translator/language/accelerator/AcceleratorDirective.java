/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator;

import cx2x.translator.common.ClawConstant;

import java.util.ArrayList;
import java.util.List;

/**
 * Enumeration that define the possible accelerator directive supported.
 * Currently OpenACC, OpenMP and NONE are available.
 *
 * @author clementval
 */
public enum AcceleratorDirective {
  NONE(ClawConstant.DIRECTIVE_NONE),
  OPENACC(ClawConstant.DIRECTIVE_OPENACC),
  OPENMP(ClawConstant.DIRECTIVE_OPENMP);

  private final String code;

  AcceleratorDirective(String code) {
    this.code = code;
  }

  public static List<String> availableDirectiveLanguage() {
    List<String> codes = new ArrayList<>();
    for(AcceleratorDirective t : AcceleratorDirective.values()) {
      codes.add(t.code);
    }
    return codes;
  }

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. NONE otherwise.
   */
  public static AcceleratorDirective fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value.toLowerCase()) {
      case ClawConstant.DIRECTIVE_NONE:
        return NONE;
      case ClawConstant.DIRECTIVE_OPENACC:
        return OPENACC;
      case ClawConstant.DIRECTIVE_OPENMP:
        return OPENMP;
      default:
        return NONE;
    }
  }

  /**
   * Get the corresponding directive prefix for a given directive primitive
   * language.
   *
   * @param directive Current directive primitive language.
   * @return The corresponding prefix. Null if language is not known.
   */
  public static String getPrefix(AcceleratorDirective directive) {
    if(directive == null) {
      return null;
    }
    switch(directive) {
      case OPENACC:
        return ClawConstant.OPENACC_PREFIX;
      case OPENMP:
        return ClawConstant.OPENMP_PREFIX;
      default:
        return null;
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
      case OPENACC:
        return ClawConstant.DIRECTIVE_OPENACC;
      case OPENMP:
        return ClawConstant.DIRECTIVE_OPENMP;
      default:
        return ClawConstant.DIRECTIVE_NONE;
    }
  }
}

