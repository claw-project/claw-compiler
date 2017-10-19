/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator.generator;

import cx2x.translator.common.Utility;
import cx2x.translator.config.Configuration;
import cx2x.translator.language.accelerator.AcceleratorDirective;
import cx2x.translator.language.base.Target;

import java.util.List;

/**
 * OpenMP base accelerator directive generator. Implements everything that is
 * common for host and device target.
 *
 * @author clementval
 */
public class OpenMp extends AcceleratorGenerator {

  private static final String OPENMP_PREFIX = "omp";
  private static final String OPENMP_DECLARE = "declare";
  private static final String OPENMP_TARGET = "target";
  private static final String OPENMP_PARALLEL = "parallel";
  private static final String OPENMP_PRIVATE = "private";
  private static final String OPENMP_DO = "do";
  private static final String OPENMP_END = "end";

  /**
   * Constructs a new object with the given target.
   *
   * @param config Configuration information object.
   */
  public OpenMp(Configuration config) {
    super(config);
  }

  @Override
  public String getPrefix() {
    return OPENMP_PREFIX;
  }

  @Override
  public String[] getStartParallelDirective(String clauses) {
    // TODO handle possible clauses
    if(getConfiguration().getCurrentTarget() == Target.GPU) {
      //!$omp target
      //!$omp parallel
      return new String[]{
          String.format(FORMAT2, OPENMP_PREFIX, OPENMP_TARGET),
          String.format(FORMAT2, OPENMP_PREFIX, OPENMP_PARALLEL)
      };
    } else {
      //!$omp parallel
      return new String[]{
          String.format(FORMAT2, OPENMP_PREFIX, OPENMP_PARALLEL)
      };
    }
  }

  @Override
  public String[] getEndParallelDirective() {
    if(getConfiguration().getCurrentTarget() == Target.GPU) {
      //!$omp end parallel
      //!$omp end target
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_PARALLEL),
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_TARGET)
      };
    } else {
      //!$omp end parallel
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_PARALLEL)
      };
    }
  }

  @Override
  public String[] getSingleDirective(String clause) {
    //!$omp <clause>
    return new String[]{
        String.format(FORMAT2, OPENMP_PREFIX, clause)
    };
  }

  @Override
  public String getParallelKeyword() {
    return OPENMP_PARALLEL;
  }

  @Override
  public String getPrivateClause(String var) {
    return String.format(FORMATPAR, OPENMP_PRIVATE, var);
  }

  @Override
  public String getPrivateClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    return String.format(FORMATPAR, OPENMP_PRIVATE, Utility.join(",", vars));
  }

  @Override
  public String[] getRoutineDirective(boolean seq) {
    // TODO check
    return new String[]{
        String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DECLARE, OPENMP_TARGET)
    };
  }

  @Override
  public boolean isCompileGuard(String rawDirective) {
    return rawDirective.toLowerCase().startsWith(OPENMP_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public AcceleratorDirective getDirectiveLanguage() {
    return AcceleratorDirective.OPENMP;
  }

  @Override
  public String[] getStartDataRegion(List<String> clauses) {
    return null; // TODO OpenMP 4.5
  }

  @Override
  public String[] getEndDataRegion() {
    return null; // TODO OpenMP 4.5
  }

  @Override
  public String getSequentialClause() {
    return null; // TODO OpenMP 4.5
  }

  @Override
  public String[] getStartLoopDirective(int value, boolean seq,
                                           boolean naked, String clauses)
  {
    // TODO CPU/GPU difference
    // TODO handle seq argument
    // TODO handle clauses
    //!$omp do
    return new String[]{
        String.format(FORMAT2, OPENMP_PREFIX, OPENMP_DO)
    };
  }

  @Override
  public String[] getEndLoopDirective() {
    // TODO CPU/GPU difference
    //!$omp end do
    return new String[]{
        String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_DO)
    };
  }

}