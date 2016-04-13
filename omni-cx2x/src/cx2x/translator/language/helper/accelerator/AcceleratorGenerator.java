/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.common.Constant;

/**
 * Basic accelerator directive generator that does nothing. Used when
 * AcceleratorDirective.NONE is set.
 *
 * TODO interface might need some refinments when we have a better idea of
 * TODO OpenACC vs OpenMP
 *
 * @author clementval
 */
class AcceleratorGenerator {

  /**
   * Get the start pragma to define a parallel accelerated region.
   * @return String value that represents the pragma.
   */
  protected String getStartParellelDirective(){
    return Constant.EMPTY_STRING;
  }

  /**
   * Get the end pragma to define a parallel accelerated region.
   * @return String value that represents the pragma.
   */
  protected String getEndParellelDirective(){
    return Constant.EMPTY_STRING;
  }

  /**
   * Get formatted pragma defined by the accelerator directive prefix and the
   * given clauses.
   * @param clause Clauses to append to the accelerator directive prefix
   * @return String value that represents the pragma.
   */
  protected String getSingleDirective(String clause){
    return Constant.EMPTY_STRING;
  }

  /**
   * Get the parallel keyword for a given accelerator language.
   * @return The corresponding parallel keyword.
   */
  protected String getParallelKeyword(){
    return Constant.EMPTY_STRING;
  }
}
