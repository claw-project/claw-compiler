/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.accelerator.generator;

import cx2x.translator.config.Configuration;
import cx2x.translator.language.accelerator.AcceleratorDirective;

import java.util.List;

/**
 * Dummy accelerator directive generator. Generates nothing.
 *
 * @author clementval
 */
public class AcceleratorNone extends AcceleratorGenerator {

  /**
   * Constructs a new object with the given target.
   *
   * @param config Configuration information object.
   */
  public AcceleratorNone(Configuration config) {
    super(config);
  }

  @Override
  public String getPrefix() {
    return null;
  }

  @Override
  public String[] getStartParallelDirective(String clauses) {
    return null;
  }

  @Override
  public String[] getEndParallelDirective() {
    return null;
  }

  @Override
  public String[] getStartLoopDirective(int value, boolean seq,
                                           boolean naked, String clauses)
  {
    return null;
  }

  @Override
  public String[] getEndLoopDirective() {
    return null;
  }

  @Override
  public String[] getSingleDirective(String clause) {
    return null;
  }

  @Override
  public String getParallelKeyword() {
    return null;
  }

  @Override
  public String getPrivateClause(String var) {
    return null;
  }

  @Override
  public String getPrivateClause(List<String> vars) {
    return "";
  }

  @Override
  public String[] getRoutineDirective(boolean seq) {
    return null;
  }

  @Override
  public boolean isCompileGuard(String rawDirective) {
    return false;
  }

  @Override
  public AcceleratorDirective getDirectiveLanguage() {
    return AcceleratorDirective.NONE;
  }

  @Override
  public String[] getStartDataRegion(List<String> clauses) {
    return null;
  }

  @Override
  public String[] getEndDataRegion() {
    return null;
  }

  @Override
  public String getSequentialClause() {
    return null;
  }

}
