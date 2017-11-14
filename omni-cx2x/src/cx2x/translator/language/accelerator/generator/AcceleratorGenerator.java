/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator.generator;

import cx2x.translator.language.accelerator.CompilerDirective;
import cx2x.translator.language.base.ClawDMD;
import cx2x.xcodeml.xnode.Xcode;

import java.util.ArrayList;
import java.util.List;

/**
 * Interface for accelerator directive generator.
 * <p>
 * TODO interface might need some refinements when we have a better idea of
 * TODO OpenACC vs OpenMP
 *
 * @author clementval
 */
public abstract class AcceleratorGenerator {

  static final String COMPILE_GUARD = "claw-guard";
  static final String FORMATPAR = "%s(%s)";
  static final String FORMAT2 = "%s %s";
  static final String FORMAT3 = "%s %s %s";
  static final String FORMAT4 = "%s %s %s %s";
  static final String FORMAT5 = "%s %s %s %s %s";

  /**
   * Constructs a new object with the given target.
   */
  public AcceleratorGenerator() {
  }

  /**
   * Get the prefix for the current accelerator language.
   *
   * @return Language prefix.
   */
  public abstract String getPrefix();

  /**
   * Get the start pragma to define a parallel accelerated region.
   *
   * @return String value that represents the pragma.
   */
  public abstract String[] getStartParallelDirective(String clauses);

  /**
   * Get the end pragma to define a parallel accelerated region.
   *
   * @return String value that represents the pragma.
   */
  public abstract String[] getEndParallelDirective();

  /**
   * Get the formatted directive to start the parallelization of a loop.
   *
   * @param value Collapse value. if greater than 0, a collapse clause will be
   *              added to the construct.
   * @param seq   If true, loop should be executed in a sequential mode.
   * @param naked If true, simple directive is generated without special
   *              clauses.
   * @return String value that represents the start of a parallelized loop.
   */
  public abstract String[] getStartLoopDirective(int value, boolean seq,
                                                 boolean naked,
                                                 String clauses);

  /**
   * Get the formatted directive to end the parallelization of a loop.
   *
   * @return String value that represents the start of a parallelized loop.
   */
  public abstract String[] getEndLoopDirective();

  /**
   * Get formatted pragma defined by the accelerator directive prefix and the
   * given clauses.
   *
   * @param clause Clauses to append to the accelerator directive prefix
   * @return String value that represents the pragma.
   */
  public abstract String[] getSingleDirective(String clause);

  /**
   * Get the parallel keyword for a given accelerator language.
   *
   * @return The corresponding parallel keyword.
   */
  public abstract String getParallelKeyword();

  /**
   * Return construction of the clause for a private variable.
   *
   * @param var Variable name that will be inserted in the generated clause.
   * @return An accelerator language specific private clause with the var.
   */
  public abstract String getPrivateClause(String var);

  /**
   * Return construction of the clause for a list of private variables.
   *
   * @param vars List of variables name that will be inserted in the generated
   *             clause.
   * @return An accelerator language specific private clause with the list of
   * variables.
   */
  public abstract String getPrivateClause(List<String> vars);

  /**
   * Return construction of the clause for a list of present variables.
   *
   * @param vars List of variables name that will be inserted in the generated
   *             clause.
   * @return An accelerator language specific present clause with the list of
   * variables. If the list is null or empty, the implementation returns an
   * empty string.
   */
  public String getPresentClause(List<String> vars) {
    return "";
  }

  /**
   * Return construction of the clause for a list of created variables.
   *
   * @param vars List of variables name that will be inserted in the generated
   *             clause.
   * @return An accelerator language specific create clause with the list of
   * variables. If the list is null or empty, the implementation returns an
   * empty string.
   */
  public String getCreateClause(List<String> vars) {
    return "";
  }

  /**
   * Return the formatted directive to be inserted in a subroutine/function
   * definition.
   *
   * @param seq Apply sequential mode to the routine directive
   * @return Routine directive.
   */
  public abstract String[] getRoutineDirective(boolean seq);

  /**
   * Check whether the raw directive is a CLAW compile guard that must be
   * removed.
   *
   * @param rawDirective The raw directive without any preprocessing.
   * @return True if it is a CLAW compile guard. False otherwise.
   */
  public abstract boolean isCompileGuard(String rawDirective);

  /**
   * Get the target of the current generator.
   *
   * @return Current target as an accelerator directive enumeration value.
   */
  public abstract CompilerDirective getDirectiveLanguage();

  /**
   * Get the start pragma to define the start of an accelerator data region.
   *
   * @return String value that represents the pragma.
   */
  public abstract String[] getStartDataRegion(List<String> clauses);

  /**
   * Get the end pragma to define the end of an accelerator data region.
   *
   * @return String value that represents the pragma.
   */
  public abstract String[] getEndDataRegion();

  /**
   * Get the corresponding clause to have a sequential execution of an
   * accelerated region.
   *
   * @return String value that represents the clause.
   */
  public abstract String getSequentialClause();

  /**
   * Get the list of unsupported statements in an accelerator region.
   *
   * @return List of Xcode opcode of unsupported statements.
   */
  public List<Xcode> getUnsupportedStatements() {
    return new ArrayList<>();
  }

  /**
   * Get the list of skipped statements before accelerator region.
   *
   * @return List of Xcode opcode of supported statements that are skipped.
   */
  public List<Xcode> getSkippedStatementsInPreamble() {
    return new ArrayList<>();
  }

  /**
   * Get the list of skipped statements after accelerator region.
   *
   * @return List of Xcode opcode of supported statements that are skipped.
   */
  public List<Xcode> getSkippedStatementsInEpilogue() {
    return new ArrayList<>();
  }

  /**
   * Get directive for updating accelerator or host memory with the given
   * variables.
   *
   * @param direction Direction of the update.
   * @param vars      List of variables.
   * @return String value that represents the directive. Null if no directive
   * generated.
   */
  public String[] getUpdateClause(ClawDMD direction, List<String> vars) {
    return null;
  }
}
