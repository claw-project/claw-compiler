/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.common;

/**
 * Contains common constants values of the CLAW XcodeML to XcodeML translator
 *
 * @author clementval
 */
public class ClawConstant {

  public static final String EMPTY_STRING = "";
  public static final String OPENACC_PREFIX = "acc";
  public static final String OPENMP_PREFIX = "omp";
  public static final int OPENACC_PREFIX_LENGTH = 6; // "!$acc "
  public static final String CONTINUATION_LINE_SYMBOL = "&";
  public static final String DEFAULT_STEP_VALUE = "1";
  public static final String DEFAULT_LOWER_BOUND = "1";
  public static final String ITER_PREFIX = "iter_";
  public static final String CLAW = "claw";
  public static final int INDENT_OUTPUT = 2; // Number of spaces for indent

  public static final String EXTRACTION_SUFFIX = "_extracted";

  public static final String CLAW_MOD_SUFFIX = ".claw";

  // CLAW attribute
  public static final String IS_CLAW = "is_claw";
  public static final String OVER = "over";

  // Over position constant value
  public static final String BEFORE = "before";
  public static final String MIDDLE = "middle";
  public static final String AFTER = "after";

  // Available targets
  public static final String TARGET_CPU = "cpu";
  public static final String TARGET_GPU = "gpu";
  public static final String TARGET_MIC = "mic";

  // Available accelerator directive primitives
  public static final String DIRECTIVE_NONE = "none";
  public static final String DIRECTIVE_OPENACC = "openacc";
  public static final String DIRECTIVE_OPENMP = "openmp";

}
