/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.TatsuConstant;

import java.util.ArrayList;
import java.util.List;

/**
 * Enumeration that define the possible directive directive supported.
 * Currently OpenACC, OpenMP and NONE are available.
 *
 * @author clementval
 */
public enum CompilerDirective {
  NONE(TatsuConstant.DIRECTIVE_NONE, ""),
  OPENACC(TatsuConstant.DIRECTIVE_OPENACC, TatsuConstant.OPENACC_PREFIX),
  OPENMP(TatsuConstant.DIRECTIVE_OPENMP, TatsuConstant.OPENMP_PREFIX),
  CLAW(TatsuConstant.DIRECTIVE_CLAW, TatsuConstant.CLAW_PREFIX);

  private final String _code;
  private final String _prefix;

  CompilerDirective(String code, String prefix) {
    _code = code;
    _prefix = prefix;
  }

  public static List<String> availableDirectiveLanguage() {
    List<String> codes = new ArrayList<>();
    for(CompilerDirective t : CompilerDirective.values()) {
      codes.add(t.getCode());
    }
    return codes;
  }

  /**
   * Get enum value from a string.
   *
   * @param value Code value for the enumeration.
   * @return The enumeration value if matches. NONE otherwise.
   */
  public static CompilerDirective fromString(String value) {
    if(value == null) {
      return NONE;
    }
    switch(value.toLowerCase()) {
      case TatsuConstant.DIRECTIVE_NONE:
        return NONE;
      case TatsuConstant.DIRECTIVE_OPENACC:
      case TatsuConstant.DIRECTIVE_SHORT_OPENACC:
        return OPENACC;
      case TatsuConstant.DIRECTIVE_OPENMP:
      case TatsuConstant.DIRECTIVE_SHORT_OPENMP:
        return OPENMP;
      default:
        return NONE;
    }
  }

  /**
   * Get the compiler directive code name.
   *
   * @return Compiler directive code name.
   */
  public String getCode() {
    return _code;
  }

  /**
   * Get the compiler directive prefix.
   *
   * @return Compiler directive prefix.
   */
  public String getPrefix() {
    return _prefix;
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
        return TatsuConstant.DIRECTIVE_OPENACC;
      case OPENMP:
        return TatsuConstant.DIRECTIVE_OPENMP;
      default:
        return TatsuConstant.DIRECTIVE_NONE;
    }
  }
}

