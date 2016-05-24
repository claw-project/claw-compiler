/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.language.helper.target.Target;
import cx2x.translator.misc.Utility;

import java.util.List;

/**
 * OpenACC specific accelerator directive generator.
 *
 * @author clementval
 */
class OpenAcc extends AcceleratorGenerator {

  private static final String OPENACC_COLLAPSE = "collapse";
  private static final String OPENACC_END = "end";
  private static final String OPENACC_LOOP = "loop";
  private static final String OPENACC_PARALLEL = "parallel";
  private static final String OPENACC_PREFIX = "acc";
  private static final String OPENACC_PRIVATE = "private";
  private static final String OPENACC_ROUTINE = "routine";

  /**
   * Constructs a new object with the given target.
   *
   * @param target Target for which the directive must be generated.
   */
  protected OpenAcc(Target target) {
    super(target);
  }


  @Override
  protected String getPrefix(){
    return OPENACC_PREFIX;
  }

  @Override
  protected String getStartParellelDirective() {
    return String.format(FORMAT2, OPENACC_PREFIX, OPENACC_PARALLEL);
  }

  @Override
  protected String getEndParellelDirective() {
    return String.format(FORMAT3,
        OPENACC_PREFIX, OPENACC_END, OPENACC_PARALLEL);
  }

  @Override
  protected String getSingleDirective(String clause) {
    return String.format(FORMAT2, OPENACC_PREFIX, clause);
  }

  @Override
  protected String getParallelKeyword(){
    return OPENACC_PARALLEL;
  }

  @Override
  protected String getPrivateClause(String var) {
    return String.format(FORMATPAR, OPENACC_PRIVATE, var);
  }

  @Override
  protected String getPrivateClause(List<String> vars) {
    return String.format(FORMATPAR, OPENACC_PRIVATE, Utility.join(",", vars));
  }

  @Override
  protected String getRoutineDirective(){
    return String.format(FORMAT2, OPENACC_PREFIX, OPENACC_ROUTINE);
  }

  @Override
  public boolean isCompileGuard(String rawDirective){
    return rawDirective.toLowerCase().startsWith(OPENACC_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public AcceleratorDirective getDirectiveLanguage(){
    return AcceleratorDirective.OPENACC;
  }

  @Override
  protected String getStartLoopDirective(int value) {
    if(value > 1){
      return String.format(FORMAT3, OPENACC_PREFIX, OPENACC_LOOP,
          String.format("%s(%d)", OPENACC_COLLAPSE, value));
    } else {
      return String.format(FORMAT2, OPENACC_PREFIX, OPENACC_LOOP);
    }
  }

  @Override
  protected String getEndLoopDirective() {
    return null;
  }
}
