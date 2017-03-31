/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.utility;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
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
   * XcodeMLWorkaround ctor.
   *
   * @param directive The directive that triggered the transformation. In case
   *                  of this specific transformation, there is no directive.
   */
  public XcodeMLWorkaround(ClawLanguage directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
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
  public void transform(XcodeProgram xcodeml, Transformer transformer,
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
