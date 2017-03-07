/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.common.Utility;
import cx2x.translator.config.Configuration;
import xcodeml.util.XmOption;

import java.util.List;

/**
 * OpenACC specific accelerator directive generator.
 *
 * @author clementval
 */
class OpenAcc extends AcceleratorGenerator {

  private static final String OPENACC_COLLAPSE = "collapse";
  private static final String OPENACC_DATA = "data";
  private static final String OPENACC_END = "end";
  private static final String OPENACC_LOOP = "loop";
  private static final String OPENACC_PARALLEL = "parallel";
  private static final String OPENACC_PREFIX = "acc";
  private static final String OPENACC_PRIVATE = "private";
  private static final String OPENACC_PRESENT = "present";
  private static final String OPENACC_ROUTINE = "routine";
  private static final String OPENACC_SEQUENTIAL = "seq";

  /**
   * Constructs a new object with the given target.
   *
   * @param config Configuration information object.
   */
  OpenAcc(Configuration config) {
    super(config);
  }

  @Override
  protected String getPrefix() {
    return OPENACC_PREFIX;
  }

  @Override
  protected String[] getStartParallelDirective(String clauses) {
    //!$acc parallel [vector_length()] [num_gang()] [num_worker()]
    if(clauses == null || clauses.isEmpty()) {
      return new String[]{
          String.format(FORMAT2, OPENACC_PREFIX, OPENACC_PARALLEL)
      };
    }
    return new String[]{
        String.format(FORMAT2, OPENACC_PREFIX, OPENACC_PARALLEL) + " " +
            clauses
    };
  }

  @Override
  protected String[] getEndParallelDirective() {
    //!$acc end parallel
    return new String[]{
        String.format(FORMAT3, OPENACC_PREFIX, OPENACC_END, OPENACC_PARALLEL)
    };
  }

  @Override
  protected String[] getSingleDirective(String clause) {
    //!$acc <clause>
    return new String[]{
        String.format(FORMAT2, OPENACC_PREFIX, clause)
    };
  }

  @Override
  protected String getParallelKeyword() {
    return OPENACC_PARALLEL;
  }

  @Override
  protected String getPrivateClause(String var) {
    return String.format(FORMATPAR, OPENACC_PRIVATE, var);
  }

  @Override
  protected String getPrivateClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    if(XmOption.isDebugOutput()) {
      System.out.println("OpenACC: generate private clause for: " +
          Utility.join(",", vars));
    }
    return String.format(FORMATPAR, OPENACC_PRIVATE, Utility.join(",", vars));
  }

  @Override
  protected String getPresentClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    if(XmOption.isDebugOutput()) {
      System.out.println("OpenACC: generate present clause for: " +
          Utility.join(",", vars));
    }
    return String.format(FORMATPAR, OPENACC_PRESENT, Utility.join(",", vars));
  }

  @Override
  protected String[] getRoutineDirective(boolean seq) {
    //!$acc routine
    if(seq) {
      return new String[]{
          String.format(FORMAT3, OPENACC_PREFIX, OPENACC_ROUTINE,
              getSequentialClause())
      };
    } else {
      return new String[]{
          String.format(FORMAT2, OPENACC_PREFIX, OPENACC_ROUTINE)
      };
    }
  }

  @Override
  public boolean isCompileGuard(String rawDirective) {
    return rawDirective.toLowerCase().startsWith(OPENACC_PREFIX) &&
        rawDirective.toLowerCase().contains(COMPILE_GUARD);
  }

  @Override
  public AcceleratorDirective getDirectiveLanguage() {
    return AcceleratorDirective.OPENACC;
  }

  @Override
  public String[] getStartDataRegion(String clauses) {
    //!$acc data
    return new String[]{
        String.format(FORMAT2, OPENACC_PREFIX, OPENACC_DATA) + " " + clauses
    };
  }

  @Override
  public String[] getEndDataRegion() {
    //!$acc end data
    return new String[]{
        String.format(FORMAT3, OPENACC_PREFIX, OPENACC_END, OPENACC_DATA)
    };
  }

  @Override
  public String getSequentialClause() {
    return OPENACC_SEQUENTIAL;
  }

  @Override
  protected String[] getStartLoopDirective(int value, boolean seq) {
    if(value > 1) {
      //!$acc loop collapse(<value>)
      // TODO do it differently
      if(seq) {
        return new String[]{
            String.format(FORMAT4, OPENACC_PREFIX, OPENACC_LOOP,
                getSequentialClause(),
                String.format("%s(%d)", OPENACC_COLLAPSE, value))
        };
      } else {
        return new String[]{
            String.format(FORMAT3, OPENACC_PREFIX, OPENACC_LOOP,
                String.format("%s(%d)", OPENACC_COLLAPSE, value))
        };
      }
    } else {
      //!$acc loop
      if(seq) {
        return new String[]{
            String.format(FORMAT3, OPENACC_PREFIX, OPENACC_LOOP,
                getSequentialClause())
        };
      } else {
        return new String[]{
            String.format(FORMAT2, OPENACC_PREFIX, OPENACC_LOOP)
        };
      }

    }
  }

  @Override
  protected String[] getEndLoopDirective() {
    return null;
  }
}
