/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.utility;

import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.XfunctionDefinition;

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
    List<XfunctionDefinition> definitions = xcodeml.getAllFctDef();
    for(XfunctionDefinition fct : definitions) {
      fct.getDeclarationTable().checkOrder(fct);
    }
  }
}
