/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.language.helper.target.Target;
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
   * @param target Target for which the directive must be generated.
   */
  AcceleratorNone(Target target) {
    super(target);
  }

  @Override
  protected String getPrefix() {
    return null;
  }

  @Override
  protected String getStartParallelDirective() {
    return null;
  }

  @Override
  protected String getStartLoopDirective(int value) {
    return null;
  }

  @Override
  protected String getEndLoopDirective() {
    return null;
  }

  @Override
  protected String getEndParallelDirective() {
    return null;
  }

  @Override
  protected String getSingleDirective(String clause) {
    return null;
  }

  @Override
  protected String getParallelKeyword() {
    return null;
  }

  @Override
  protected String getPrivateClause(String var) {
    return null;
  }

  @Override
  protected String getPrivateClause(List<String> vars) {
    return null;
  }

  @Override
  protected String getPresentClause(List<String> vars) {
    return null;
  }

  @Override
  protected String getRoutineDirective() {
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
  public String getStartDataRegion() {
    return null;
  }

  @Override
  public String getEndDataRegion() {
    return null;
  }

  @Override
  public String getSequentialClause() {
    return null;
  }
}
