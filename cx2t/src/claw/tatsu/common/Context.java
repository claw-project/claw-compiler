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
import claw.wani.x2t.configuration.AcceleratorConfiguration;
import claw.wani.x2t.configuration.OpenAccConfiguration;
import claw.wani.x2t.configuration.OpenMpConfiguration;

/**
 * Class holding all information needed during a translation.
 *
 * @author clementval
 */
public class Context {

  private static Context _instance = null;

  private final int _maxColumns;
  private final DirectiveGenerator _directiveGenerator;
  private final CompilerDirective _compilerDirective;
  private final Target _target;
  private final ModuleCache _moduleCache;

  /**
   * Create a new context.
   *
   * @param compilerDirective        Compiler directive to used.
   * @param target                   Target for the current translation.
   * @param acceleratorConfiguration Configuration for the accelerator
   *                                 translation.
   * @param maxColumns               Max columns.
   */
  private Context(CompilerDirective compilerDirective, Target target,
                  AcceleratorConfiguration acceleratorConfiguration,
                  int maxColumns)
  {
    if(compilerDirective == null) {
      _compilerDirective = CompilerDirective.NONE;
    } else {
      _compilerDirective = compilerDirective;
    }

    if(acceleratorConfiguration instanceof OpenAccConfiguration) {
      OpenAcc gen = new OpenAcc();
      gen.setExecutionMode(
          ((OpenAccConfiguration) acceleratorConfiguration).getMode());
      _directiveGenerator = gen;
    } else if (acceleratorConfiguration instanceof OpenMpConfiguration) {
      OpenMp gen = new OpenMp();
      gen.setExecutionMode(
          ((OpenMpConfiguration) acceleratorConfiguration).getMode());
      _directiveGenerator = gen;
    } else {
      _directiveGenerator = new DirectiveNone();
    }

    if(target == null) {
      _target = Target.NONE;
    } else {
      _target = target;
    }
    _maxColumns = maxColumns;
    _moduleCache = new ModuleCache();
  }

  /**
   * Initialize the context.
   *
   * @param compilerDirective        Compiler directive to used.
   * @param target                   Target for the current translation.
   * @param acceleratorConfiguration Configuration for the accelerator
   *                                 translation.
   * @param maxColumns               Max columns.
   */
  public static void init(CompilerDirective compilerDirective, Target target,
                          AcceleratorConfiguration acceleratorConfiguration,
                          int maxColumns)
  {
    _instance = new Context(compilerDirective, target, acceleratorConfiguration,
        maxColumns);
  }

  /**
   * Get the unique context instance.
   *
   * @return Current context instance.
   */
  public static Context get() {
    return _instance;
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

  /**
   * Check is current target is corresponding to the given one.
   *
   * @param value Target value to check against.
   * @return True if the target is identical to the given one. False otherwise.
   */
  public static boolean isTarget(Target value) {
    return (get().getTarget() == value);
  }
}
