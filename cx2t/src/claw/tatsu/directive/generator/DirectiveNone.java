/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.common.CompilerDirective;

import java.util.List;

/**
 * Dummy directive directive generator. Generates nothing.
 *
 * @author clementval
 */
public class DirectiveNone extends DirectiveGenerator {

  /**
   * Constructs a new object with the given target.
   */
  public DirectiveNone() {
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
  public CompilerDirective getDirectiveLanguage() {
    return CompilerDirective.NONE;
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
