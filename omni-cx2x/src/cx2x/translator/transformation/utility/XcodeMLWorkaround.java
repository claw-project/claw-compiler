/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.utility;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XnodeUtil;
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
public class XcodeMLWorkaround extends Transformation {

  /**
   * XcodeMLWorkaround ctor.
   * @param claw The directive that triggered the transformation. In case of
   *             this specific transformation, there is no directive.
   */
  public XcodeMLWorkaround(ClawLanguage claw) {
    super(claw);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
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
   * @param xcodeml Current XcodeML program unit.
   */
  private void checkDeclarations(XcodeProgram xcodeml){
    List<XfunctionDefinition> definitions = XnodeUtil.getAllFctDef(xcodeml);
    for(XfunctionDefinition fct : definitions){
      fct.getDeclarationTable().checkOrder(fct);
    }
  }
}
