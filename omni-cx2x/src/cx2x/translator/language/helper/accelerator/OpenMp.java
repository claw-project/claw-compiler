/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

/**
 * OpenMP specific accelerator directive generator.
 *
 * @author clementval
 */
class OpenMp extends AcceleratorGenerator {

  private static final String OPENMP_PREFIX = "omp";
  private static final String OPENMP_DECLARE = "delcare";
  private static final String OPENMP_TARGET = "target";
  private static final String OPENMP_PARALLEL = "parallel";
  private static final String OPENMP_DO = "do";
  private static final String OPENMP_END = "end";

  @Override
  protected String getPrefix(){
    return OPENMP_PREFIX;
  }

  @Override
  protected String getStartParellelDirective() {
    //!$omp target parallel do
    return String.format(FORMAT4,
        OPENMP_PREFIX, OPENMP_TARGET, OPENMP_PARALLEL, OPENMP_DO);
  }

  @Override
  public String getEndParellelDirective() {
    //!$omp end target parallel do
    return String.format(FORMAT5,
        OPENMP_PREFIX, OPENMP_END, OPENMP_TARGET, OPENMP_PARALLEL, OPENMP_DO);
  }

  @Override
  public String getSingleDirective(String clause) {
    return ""; // TODO
  }

  @Override
  protected String getParallelKeyword() {
    return ""; // TODO
  }

  @Override
  protected String getPrivateClause(String var) {
    return ""; // TODO
  }

  @Override
  protected String getRoutineDirective(){
    return String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DECLARE, OPENMP_TARGET);
  }

  @Override
  public boolean isCompileGuard(String rawDirective){
    return rawDirective.toLowerCase().startsWith(OPENMP_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public AcceleratorDirective getDirectiveLanguage(){
    return AcceleratorDirective.OPENMP;
  }

  @Override
  protected String getStartLoopDirective(int value) {
    throw new NotImplementedException();
  }

  @Override
  protected String getEndLoopDirective() {
    throw new NotImplementedException();
  }
}
