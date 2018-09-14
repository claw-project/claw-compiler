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

  @Override
  public String getPrefix() {
    return "";
  }

  @Override
  public String getPrefixCont() {
    return "";
  }

  @Override
  public String[] getStartParallelDirective(String clauses) {
    return new String[0];
  }

  @Override
  public String[] getEndParallelDirective() {
    return new String[0];
  }

  @Override
  public String[] getStartLoopDirective(int value, boolean seq,
                                        boolean naked, String clauses)
  {
    return new String[0];
  }

  @Override
  public String[] getEndLoopDirective() {
    return new String[0];
  }

  @Override
  public String[] getSingleDirective(String clause) {
    return new String[0];
  }

  @Override
  public String getParallelKeyword() {
    return "";
  }

  @Override
  public String getPrivateClause(String var) {
    return "";
  }

  @Override
  public String getPrivateClause(List<String> vars) {
    return "";
  }

  @Override
  public String[] getRoutineDirective(boolean seq) {
    return new String[0];
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
    return new String[0];
  }

  @Override
  public String[] getEndDataRegion() {
    return new String[0];
  }

  @Override
  public String getSequentialClause() {
    return "";
  }
}
