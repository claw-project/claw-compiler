/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.language.helper.target.Target;

import java.util.List;

/**
 * Interface for accelerator directive generator.
 *
 * TODO interface might need some refinements when we have a better idea of
 * TODO OpenACC vs OpenMP
 *
 * @author clementval
 */
public abstract class AcceleratorGenerator {

  private final Target _target;
  static final String COMPILE_GUARD = "claw-guard";
  static final String FORMATPAR = "%s(%s)";
  static final String FORMAT2 = "%s %s";
  static final String FORMAT3 = "%s %s %s";
  static final String FORMAT4 = "%s %s %s %s";

  /**
   * Constructs a new object with the given target.
   * @param target Target for which the directive must be generated.
   */
  AcceleratorGenerator(Target target) {
    _target = target;
  }

  /**
   * Get the associated target.
   * @return Target.
   */
  Target getTarget(){
    return _target;
  }

  /**
   * Get the prefix for the current accelerator language.
   * @return Language prefix.
   */
  protected abstract String getPrefix();

  /**
   * Get the start pragma to define a parallel accelerated region.
   * @return String value that represents the pragma.
   */
  protected abstract String getStartParallelDirective();

  /**
   * Get the formatted directive to start the parallelization of a loop.
   * @param value Collapse value. if greater than 0, a collapse clause will be
   *              added to the construct.
   * @return String value that represents the start of a parallelized loop.
   */
  protected abstract String getStartLoopDirective(int value);

  /**
   * Get the formatted directive to end the parallelization of a loop.
   * @return String value that represents the start of a parallelized loop.
   */
  protected abstract String getEndLoopDirective();

  /**
   * Get the end pragma to define a parallel accelerated region.
   * @return String value that represents the pragma.
   */
  protected abstract String getEndParallelDirective();

  /**
   * Get formatted pragma defined by the accelerator directive prefix and the
   * given clauses.
   * @param clause Clauses to append to the accelerator directive prefix
   * @return String value that represents the pragma.
   */
  protected abstract String getSingleDirective(String clause);

  /**
   * Get the parallel keyword for a given accelerator language.
   * @return The corresponding parallel keyword.
   */
  protected abstract String getParallelKeyword();

  /**
   * Return construction of the clause for a private variable.
   * @param var Variable name that will be inserted in the generated clause.
   * @return An accelerator language specific private clause with the var.
   */
  protected abstract String getPrivateClause(String var);

  /**
   * Return construction of the clause for a list of private variables.
   * @param vars List of variables name that will be inserted in the generated
   *             clause.
   * @return An accelerator language specific private clause with the list of
   * variables.
   */
  protected abstract String getPrivateClause(List<String> vars);

  /**
   * Return construction of the clause for a list of present variables.
   * @param vars List of variables name that will be inserted in the generated
   *             clause.
   * @return An accelerator language specific present clause with the list of
   * variables. If the list is null or empty, the implementation returns an
   * empty string.
   */
  protected abstract String getPresentClause(List<String> vars);

  /**
   * Return the formatted directive to be inserted in a subroutine/function
   * definition.
   * @return Routine directive.
   */
  protected abstract String getRoutineDirective();


  /**
   * Check whether the raw directive is a CLAW compile guard that must be
   * removed.
   * @param rawDirective The raw directive without any preprocessing.
   * @return True if it is a CLAW compile guard. False otherwise.
   */
  public abstract boolean isCompileGuard(String rawDirective);

  /**
   * Get the target of the current generator.
   * @return Current target as an accelerator directive enumeration value.
   */
  public abstract AcceleratorDirective getDirectiveLanguage();

  /**
   * Get the start pragma to define the start of an accelerator data region.
   * @return String value that represents the pragma.
   */
  public abstract String getStartDataRegion();

  /**
   * Get the end pragma to define the end of an accelerator data region.
   * @return String value that represents the pragma.
   */
  public abstract String getEndDataRegion();

  /**
   * Get the corresponding clause to have a sequential execution of an
   * accelerated region.
   * @return String value that represents the clause.
   */
  public abstract String getSequentialClause();

}
