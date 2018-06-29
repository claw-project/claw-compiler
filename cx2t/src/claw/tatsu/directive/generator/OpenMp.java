/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.common.*;
import claw.tatsu.directive.common.DataMovement;
import claw.tatsu.directive.generator.openmp.OpenMpExecutionMode;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.wani.x2t.configuration.Configuration;

import java.util.Arrays;
import java.util.List;

/**
 * OpenMP base directive directive generator. Implements everything that is
 * common for host and device target.
 *
 * @author clementval
 */
public class OpenMp extends DirectiveGenerator {

  public static final String OPENMP_DEBUG_PREFIX = "CLAW-OpenMP:";
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
  private static final String OPENMP_SCHEDULE_KIND = "static";
  private static final String OPENMP_PARALLEL = "parallel";
  private static final String OPENMP_SEQUENTIAL = "single";
  private static final String OPENMP_MAP = "map";
  private static final String OPENMP_FROM = "from";
  private static final String OPENMP_TO = "to";
  private static final String OPENMP_UPDATE = "update";
  private static final String OPENMP_PRIVATE = "private";
  private static final String OPENMP_FIRSTPRIVATE = "firstprivate";
  private static final String OPENMP_ALLOC = "alloc";
  private static final String OPENMP_DO = "do";
  private static final String OPENMP_END = "end";

  private OpenMpExecutionMode _mode;

  /**
   * Constructs a new object with the given target.
   */
  public OpenMp() {
    super();
  }

  public void setExecutionMode(OpenMpExecutionMode mode) {
    _mode = mode;
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
      if(clauses == null) {
        clauses = new String();
      } else {
        clauses = clauses.trim(); // Useless ?
      }
      clauses += String.format("%s(%d)", OPENMP_THREADS_LIMIT,
          Configuration.get().openMP().getNumThreads());
      clauses += String.format(" %s(%d)", OPENMP_NUM_TEAMS,
          Configuration.get().openMP().getNumTeams());

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
    Message.debug(String.format(
        "%s generate private clause for (%d variables): %s",
        OPENMP_DEBUG_PREFIX, vars.size(), Utility.join(",", vars)));
    return String.format(FORMATPAR, OPENMP_PRIVATE, Utility.join(",", vars));
  }

  @Override
  public String getCreateClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    Message.debug(String.format(
        "%s generate map(alloc:x) clause for (%d variables): %s",
        OPENMP_DEBUG_PREFIX, vars.size(), Utility.join(",", vars)));
    return String.format(FORMATPAR, OPENMP_MAP,
        String.format("%s:%s", OPENMP_ALLOC, Utility.join(",", vars)));
  }

  @Override
  public String[] getRoutineDirective(boolean seq) {
    if(seq) {
      return new String[]{
          String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DECLARE, OPENMP_TARGET),
          String.format(FORMAT3, OPENMP_PREFIX, getSequentialClause())
          // TODO: Check // Peclat
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
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_DECLARE,
              OPENMP_TARGET),
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END,
              getSequentialClause()) // TODO: Check // Peclat
      };
    } else {
      return new String[]{
          String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_DECLARE,
              OPENMP_TARGET)
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
        String.format(FORMAT4, OPENMP_PREFIX, OPENMP_END, OPENMP_TARGET,
            OPENMP_DATA)
    };
  }

  @Override
  public String getSequentialClause() {
    // !$omp single / !$omp end single
    return OPENMP_SEQUENTIAL;
    // TODO: For OpenMP this is a region and not a clause
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

    clauses = clauses.trim(); // Useless ?

    if(value > 1) {
      clauses += String.format("%s(%d) ", OPENMP_COLLAPSE, value);
    }
    String scheduler = String.format("%s(%s, %d)", OPENMP_DIST_SCHEDULE,
        OPENMP_SCHEDULE_KIND, Configuration.get().openMP().getSchedulerChunkSize());

    // TODO handle wrong clauses ?
    if(Context.get().getTarget() == Target.GPU) {
      //!$omp distribute [collapse(#)] [dist_schedule(static,#)]
      if(clauses == null || clauses.isEmpty()) {
        return new String[]{
            String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DISTRIBUTE, scheduler),
        };
      } else {
        return new String[]{
            String.format(FORMAT3, OPENMP_PREFIX, OPENMP_DISTRIBUTE, scheduler) + " "
                + clauses,
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

  @Override
  public List<Xcode> getUnsupportedStatements() {
    return Arrays.asList(
        Xcode.F_ALLOCATE_STATEMENT, Xcode.F_DEALLOCATE_STATEMENT
    );
  }

  @Override
  public List<Xcode> getSkippedStatementsInPreamble() {
    return Arrays.asList(
        Xcode.F_IF_STATEMENT, Xcode.F_ALLOCATE_STATEMENT
    );
  }

  @Override
  public List<Xcode> getSkippedStatementsInEpilogue() {
    return Arrays.asList(
        Xcode.F_IF_STATEMENT, Xcode.F_DEALLOCATE_STATEMENT,
        Xcode.F_PRAGMA_STATEMENT
    );
  }

  @Override
  public String[] getUpdateClause(DataMovement direction, List<String> vars) {
    //!$omp target update from/to(<vars>)
    if(vars == null || vars.isEmpty()) {
      return null;
    }
    Message.debug(OPENMP_DEBUG_PREFIX + "generate update " +
        (direction == DataMovement.DEVICE ? OPENMP_TO : OPENMP_FROM) +
        " clause for: " + Utility.join(",", vars));
    String updates = String.format(FORMATPAR, direction == DataMovement.DEVICE ?
        OPENMP_TO : OPENMP_FROM, Utility.join(",", vars));
    return new String[]{
        String.format(FORMAT4, OPENMP_PREFIX, OPENMP_TARGET, OPENMP_UPDATE, updates)
    };
  }
}
