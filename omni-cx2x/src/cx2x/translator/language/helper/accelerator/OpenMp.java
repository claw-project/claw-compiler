/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

/**
 * OpenMP specific accelerator directive generator.
 *
 * TODO all
 *
 * @author clementval
 */
class OpenMp extends AcceleratorGenerator {

  private static final String OPENMP_PREFIX = "omp";
  private static final String OPENMP_DECLARE = "delcare";
  private static final String OPENMP_TARGET = "target";

  @Override
  protected String getPrefix(){
    return "";
  }

  @Override
  protected String getStartParellelDirective() {
    return "";
  }

  @Override
  public String getEndParellelDirective() {
    return "";
  }

  @Override
  public String getSingleDirective(String clause) {
    return "";
  }

  @Override
  protected String getParallelKeyword() {
    return "";
  }

  @Override
  protected String getPrivateClause(String var) {
    return "";
  }

  @Override
  protected String getAcceleratorRoutineDirective(){
    return OPENMP_PREFIX + " " + OPENMP_DECLARE + " " + OPENMP_TARGET;
  }

  @Override
  public boolean isCompileGuard(String rawDirective){
    return rawDirective.toLowerCase().startsWith(OPENMP_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public AcceleratorDirective getTarget(){
    return AcceleratorDirective.OPENACC;
  }
}
