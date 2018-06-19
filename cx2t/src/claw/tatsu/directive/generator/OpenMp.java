/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.common.Utility;

import java.util.List;

/**
 * OpenMP base directive directive generator. Implements everything that is
 * common for host and device target.
 *
 * @author clementval
 */
public class OpenMp extends DirectiveGenerator {

  private static final String OPENMP_PREFIX = "omp";
  private static final String OPENMP_DECLARE = "declare";
  private static final String OPENMP_TARGET = "target";
  private static final String OPENMP_DATA = "data";
  private static final String OPENMP_TEAMS = "teams";
  private static final String OPENMP_THREADS_LIMIT = "thread_limit";
  private static final String OPENMP_NUM_TEAMS = "num_teams";
  private static final String OPENMP_DISTRIBUTE = "distribute";
  private static final String OPENMP_COLLAPSE = "collapse";
  private static final String OPENMP_DIST_SCHEDULE = "dist_schedule";
  private static final String OPENMP_PARALLEL = "parallel";
  private static final String OPENMP_SEQUENTIAL = "single";
  private static final String OPENMP_PRIVATE = "private";
  private static final String OPENMP_FIRSTPRIVATE = "firstprivate";
  private static final String OPENMP_DO = "do";
  private static final String OPENMP_END = "end";

  /**
   * Constructs a new object with the given target.
   */
  public OpenMp() {
    super();
  }

  @Override
  public String getPrefix() {
    return OPENMP_PREFIX;
  }

  @Override
  public String[] getStartParallelDirective(String clauses) {
    // TODO handle possible clauses
    if(Context.get().getTarget() == Target.GPU) {
      //!$omp target
      //!$omp teams [num_teams(#)] [thread_limit(#)]
      if(clauses == null || clauses.isEmpty()) {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_TARGET),
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_TEAMS),
        };
      } else {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_TARGET),
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_TEAMS) + " " + clauses,
        };
      }
    } else {
      //!$omp parallel
      return new String[]{
          String.format(FORMAT2, OPENMP_PREFIX, OPENMP_PARALLEL)
      };
    }
  }

  @Override
  public String[] getEndParallelDirective() {
    if(Context.get().getTarget() == Target.GPU) {
      //!$omp end teams
      //!$omp end target
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_TEAMS),
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
    if(Context.get().getTarget() == Target.GPU) {
      return OPENMP_TEAMS;
    } else {
      return OPENMP_PARALLEL;
    }
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
    if(seq) {
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DECLARE, OPENMP_TARGET),
          String.format(FORMAT3, OPENMP_PREFIX, getSequentialClause()) // TODO: Check // Peclat
      };
    } else {
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DECLARE, OPENMP_TARGET)
      };
    }
  }

  // Peclat
  public String[] getEndRoutineDirective(boolean seq) {
    if(seq) {
      return new String[]{
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_DECLARE, OPENMP_TARGET),
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, getSequentialClause()) // TODO: Check // Peclat
      };
    } else {
      return new String[]{
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_DECLARE, OPENMP_TARGET)
      };
    }

  }

  @Override
  public boolean isCompileGuard(String rawDirective) {
    return rawDirective.toLowerCase().startsWith(OPENMP_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public CompilerDirective getDirectiveLanguage() {
    return CompilerDirective.OPENMP;
  }

  @Override
  public String[] getStartDataRegion(List<String> clauses) {
    // !$omp target data
    return new String[]{
        String.format(FORMAT4, OPENMP_PREFIX, OPENMP_TARGET, OPENMP_DATA,
            Utility.join(" ", clauses)).trim()
    };
  }

  @Override
  public String[] getEndDataRegion() {
    // !$omp end target data
    return new String[]{
        String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_TARGET, OPENMP_DATA)
    };
  }

  @Override
  public String getSequentialClause() {
    // !$omp single / !$omp end single
    return OPENMP_SEQUENTIAL; // TODO: For OpenMP this is a region and not a clause
  }

  @Override
  public String[] getStartLoopDirective(int value, boolean seq,
                                        boolean naked, String clauses)
  {
    // naked is not used for OpenMP
    // serial loop is the default behaviour for OpenMP
    if(seq) {
      return null;
    }

    if(value > 0) {
      clauses = clauses.trim(); // Useless ?
      clauses += String.format("%s(%d)", OPENMP_COLLAPSE, value);
    }

    // TODO handle wrong clauses ?
    // TODO handle naked
    if(Context.get().getTarget() == Target.GPU) {
      //!$omp distribute [collapse(#)] [dist_schedule(static,#)]
      if(clauses == null || clauses.isEmpty()) {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_DISTRIBUTE),
        };
      } else {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_DISTRIBUTE) + " " + clauses,
        };
      }
    } else {
      //!$omp do [collapse(#)]
      if(clauses == null || clauses.isEmpty()) {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_DO),
        };
      } else {
        return new String[]{
            String.format(FORMAT2, OPENMP_PREFIX, OPENMP_DO) + " " + clauses,
        };
      }
    }
  }

  @Override
  public String[] getEndLoopDirective() {
    if(Context.get().getTarget() == Target.GPU) {
      //!$omp end distribute
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_DISTRIBUTE),
      };
    } else {
      //!$omp end do
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_END, OPENMP_DO),
      };
    }
  }
}
