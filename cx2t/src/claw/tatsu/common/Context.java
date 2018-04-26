/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.directive.generator.DirectiveGenerator;
import claw.tatsu.directive.generator.DirectiveNone;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.directive.generator.OpenMp;
import claw.tatsu.xcodeml.module.ModuleCache;

/**
 * @author clementval
 */
public class Context {

  private static Context _instance = null;

  private final int _maxColumns;
  private final DirectiveGenerator _directiveGenerator;
  private final CompilerDirective _compilerDirective;
  private final Target _target;
  private final ModuleCache _moduleCache;

  private Context(CompilerDirective compilerDirective, Target target,
                  int maxColumns)
  {
    if(compilerDirective == null) {
      _compilerDirective = CompilerDirective.NONE;
    } else {
      _compilerDirective = compilerDirective;
    }
    _directiveGenerator = instantiateGenerator();
    if(target == null) {
      _target = Target.NONE;
    } else {
      _target = target;
    }
    _maxColumns = maxColumns;
    _moduleCache = new ModuleCache();
  }

  public static void init(CompilerDirective compilerDirective, Target target,
                          int maxColumns)
  {
    _instance = new Context(compilerDirective, target, maxColumns);
  }

  public static Context get() {
    return _instance;
  }

  private DirectiveGenerator instantiateGenerator() {
    if(_compilerDirective == CompilerDirective.OPENACC) {
      return new OpenAcc();
    } else if(_compilerDirective == CompilerDirective.OPENMP) {
      return new OpenMp();
    } else {
      return new DirectiveNone();
    }
  }

  public int getMaxColumns() {
    return _maxColumns;
  }

  public Target getTarget() {
    return _target;
  }

  public CompilerDirective getCompilerDirective() {
    return _compilerDirective;
  }

  public DirectiveGenerator getGenerator() {
    return _directiveGenerator;
  }

  public ModuleCache getModuleCache() {
    return _moduleCache;
  }
}
