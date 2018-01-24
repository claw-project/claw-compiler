/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.internal;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.transformation.ClawTransformation;

import java.util.List;

/**
 * Transformation to fix some issues raised in XcodeML.
 *
 * @author clementval
 */
public class XcodeMLWorkaround extends ClawTransformation {

  /**
   * Default ctor as this is a translation_unit triggered transformation.
   */
  public XcodeMLWorkaround() {
    super();
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    return true;
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // Independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other) throws Exception
  {
    // Apply declaration reordering transformation
    checkDeclarations(xcodeml);
  }

  /**
   * Check the declaration table of each function definition.
   *
   * @param xcodeml Current XcodeML program unit.
   */
  private void checkDeclarations(XcodeProgram xcodeml) {
    List<FfunctionDefinition> definitions = xcodeml.getAllFctDef();
    for(FfunctionDefinition fct : definitions) {
      fct.getDeclarationTable().checkOrder(fct);
    }
  }
}
