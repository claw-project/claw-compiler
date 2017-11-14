/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.directive.generator;

import cx2x.translator.common.Message;
import cx2x.translator.common.Utility;
import cx2x.configuration.Configuration;
import cx2x.translator.directive.CompilerDirective;
import cx2x.translator.language.base.ClawDMD;
import cx2x.xcodeml.xnode.Xcode;

import java.util.Arrays;
import java.util.List;

/**
 * OpenACC specific directive directive generator.
 *
 * @author clementval
 */
public class OpenAcc extends DirectiveGenerator {

  public static final String OPENACC_DEBUG_PREFIX = "CLAW-OpenACC: ";
  private static final String OPENACC_COLLAPSE = "collapse";
  private static final String OPENACC_DATA = "data";
  private static final String OPENACC_END = "end";
  private static final String OPENACC_LOOP = "loop";
  private static final String OPENACC_PARALLEL = "parallel";
  private static final String OPENACC_PREFIX = "acc";
  private static final String OPENACC_PRIVATE = "private";
  private static final String OPENACC_PRESENT = "present";
  private static final String OPENACC_PCREATE = "pcreate";
  private static final String OPENACC_ROUTINE = "routine";
  private static final String OPENACC_SEQUENTIAL = "seq";
  private static final String OPENACC_UPDATE = "update";
  private static final String OPENACC_DEVICE = "device";
  private static final String OPENACC_HOST = "host";

  /**
   * Constructs a new object with the given target.
   */
  public OpenAcc() {
    super();
  }

  @Override
  public String getPrefix() {
    return OPENACC_PREFIX;
  }

  @Override
  public String[] getStartParallelDirective(String clauses) {
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
  public String[] getEndParallelDirective() {
    //!$acc end parallel
    return new String[]{
        String.format(FORMAT3, OPENACC_PREFIX, OPENACC_END, OPENACC_PARALLEL)
    };
  }

  @Override
  public String[] getSingleDirective(String clause) {
    //!$acc <clause>
    return new String[]{
        String.format(FORMAT2, OPENACC_PREFIX, clause)
    };
  }

  @Override
  public String getParallelKeyword() {
    return OPENACC_PARALLEL;
  }

  @Override
  public String getPrivateClause(String var) {
    return String.format(FORMATPAR, OPENACC_PRIVATE, var);
  }

  @Override
  public String getPrivateClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    Message.debug(String.format(
        "%s generate private clause for (%d variables): %s",
        OPENACC_DEBUG_PREFIX, vars.size(), Utility.join(",", vars)));
    return String.format(FORMATPAR, OPENACC_PRIVATE, Utility.join(",", vars));
  }

  @Override
  public String getPresentClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    Message.debug(String.format(
        "%s generate present clause for (%d variables): %s",
        OPENACC_DEBUG_PREFIX, vars.size(), Utility.join(",", vars)));
    return String.format(FORMATPAR, OPENACC_PRESENT, Utility.join(",", vars));
  }

  @Override
  public String getCreateClause(List<String> vars) {
    if(vars == null || vars.size() == 0) {
      return "";
    }
    Message.debug(String.format(
        "%s generate pcreate clause for (%d variables): %s",
        OPENACC_DEBUG_PREFIX, vars.size(), Utility.join(",", vars)));
    return String.format(FORMATPAR, OPENACC_PCREATE, Utility.join(",", vars));
  }

  @Override
  public String[] getRoutineDirective(boolean seq) {
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
  public CompilerDirective getDirectiveLanguage() {
    return CompilerDirective.OPENACC;
  }

  @Override
  public String[] getStartDataRegion(List<String> clauses) {
    //!$acc data
    return new String[]{
        String.format(FORMAT3, OPENACC_PREFIX, OPENACC_DATA,
            Utility.join(" ", clauses)).trim()
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
  public String[] getStartLoopDirective(int value, boolean seq,
                                        boolean naked, String clauses)
  {
    if(value > 1) {
      //!$acc loop collapse(<value>)
      // TODO do it differently
      if(seq) {
        return new String[]{
            String.format(FORMAT5, OPENACC_PREFIX, OPENACC_LOOP,
                getSequentialClause(), String.format("%s(%d)",
                    OPENACC_COLLAPSE, value), clauses).trim()
        };
      } else {
        return new String[]{
            String.format(FORMAT5, OPENACC_PREFIX, OPENACC_LOOP,
                String.format("%s(%d)", OPENACC_COLLAPSE, value),
                naked ? "" :
                    Configuration.get().openACC().getFormattedExecutionMode(),
                clauses).trim()
        };
      }
    } else {
      //!$acc loop
      if(seq) {
        return new String[]{
            String.format(FORMAT4, OPENACC_PREFIX, OPENACC_LOOP,
                getSequentialClause(), clauses).trim()
        };
      } else {
        return new String[]{
            String.format(FORMAT4, OPENACC_PREFIX, OPENACC_LOOP,
                naked ? "" :
                    Configuration.get().openACC().getFormattedExecutionMode(),
                clauses).trim()
        };
      }

    }
  }

  @Override
  public String[] getEndLoopDirective() {
    return null;
  }

  @Override
  public List<Xcode> getUnsupportedStatements() {
    return Arrays.asList(
        Xcode.FALLOCATESTATEMENT, Xcode.FDEALLOCATESTATEMENT
    );
  }

  @Override
  public List<Xcode> getSkippedStatementsInPreamble() {
    return Arrays.asList(
        Xcode.FIFSTATEMENT, Xcode.FALLOCATESTATEMENT, Xcode.FPRAGMASTATEMENT
    );
  }

  @Override
  public List<Xcode> getSkippedStatementsInEpilogue() {
    return Arrays.asList(
        Xcode.FIFSTATEMENT, Xcode.FDEALLOCATESTATEMENT, Xcode.FPRAGMASTATEMENT
    );
  }

  @Override
  public String[] getUpdateClause(ClawDMD direction, List<String> vars) {
    //!$acc update host/device(<vars>)
    if(vars == null || vars.isEmpty()) {
      return null;
    }
    Message.debug(OPENACC_DEBUG_PREFIX + "generate update " +
        (direction == ClawDMD.DEVICE ? OPENACC_DEVICE : OPENACC_HOST) +
        " clause for: " + Utility.join(",", vars));
    String updates = String.format(FORMATPAR, direction == ClawDMD.DEVICE ?
        OPENACC_DEVICE : OPENACC_HOST, Utility.join(",", vars));
    return new String[]{
        String.format(FORMAT3, OPENACC_PREFIX, OPENACC_UPDATE, updates)
    };
  }
}
