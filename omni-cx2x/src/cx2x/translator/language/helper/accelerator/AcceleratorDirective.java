/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

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

  public static List<String> availableDirectiveLanguage(){
    return Arrays.asList(none, openacc, openmp);
  }

  public static AcceleratorDirective fromString(String value){
    if(value == null){
      return NONE;
    }
    switch (value){
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
}

