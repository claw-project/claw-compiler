/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu;

/**
 * Class holding constant variable.
 *
 * @author clementval
 */
public final class TatsuConstant {

  public static final String DIRECTIVE_NONE = "none";
  public static final String DIRECTIVE_OPENACC = "openacc";
  public static final String DIRECTIVE_SHORT_OPENACC = "acc";
  public static final String DIRECTIVE_OPENMP = "openmp";
  public static final String DIRECTIVE_SHORT_OPENMP = "omp";
  public static final String DIRECTIVE_CLAW = "claw";
  public static final String OPENACC_PREFIX = "acc";
  public static final String OPENACC_PREFIX_CONT = "acc&";
  public static final String OPENMP_PREFIX = "omp";
  public static final String CLAW_PREFIX = "claw";
  // Insertion position constant value
  public static final String BEFORE = "before";
  public static final String MIDDLE = "middle";
  public static final String AFTER = "after";
  public static final String ERROR_INCOMPATIBLE =
      "Incompatible node passed as arguments";
  public static final String CONTINUATION_LINE_SYMBOL = "&";
  public static final String OPENACC_EXEC_MODE_NONE = "none";
  public static final String OPENACC_EXEC_MODE_VECTOR = "vector";
  public static final String OPENACC_EXEC_MODE_GANG = "gang";
  public static final String OPENACC_EXEC_MODE_VECTOR_GANG = "vector_gang";
  public static final String OPENACC_EXEC_MODE_GANG_VECTOR = "gang_vector";
  // Available targets
  public static final String TARGET_ARM = "arm";
  public static final String TARGET_CPU = "cpu";
  public static final String TARGET_GPU = "gpu";
  public static final String TARGET_MIC = "mic";
  public static final String TARGET_FPGA = "fpga";
  public static final String TARGET_NONE = "none";

  // Avoid instantiation of this class.
  private TatsuConstant() {
  }
}
