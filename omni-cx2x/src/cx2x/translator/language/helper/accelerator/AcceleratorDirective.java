/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.common.ClawConstant;

import java.util.Arrays;
import java.util.List;

/**
 * Enumeration that define the possible accelerator directive supported.
 * Currently OpenACC, OpenMP and NONE are available.
 *
 * @author clementval
 */
public enum AcceleratorDirective {
  NONE,
  OPENACC,
  OPENMP;

  private static final String none = "none";
  private static final String openacc = "openacc";
  private static final String openmp = "openmp";

  public static List<String> availableDirectiveLanguage() {
    return Arrays.asList(none, openacc, openmp);
  }

  public static AcceleratorDirective fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value) {
      case none:
        return NONE;
      case openacc:
        return OPENACC;
      case openmp:
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
    switch(directive) {
      case OPENACC:
        return ClawConstant.OPENACC_PREFIX;
      case OPENMP:
        return ClawConstant.OPENMP_PREFIX;
      default:
        return null;
    }
  }
}

