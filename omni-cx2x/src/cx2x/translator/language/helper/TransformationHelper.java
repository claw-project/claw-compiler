/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XbaseElement;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XdoStatement;
import xcodeml.util.XmOption;

/**
 * @author clementval
 */
public class TransformationHelper {


  /**
   *
   * @param claw
   * @param xcodeml
   * @param transformer
   * @param stmt
   */
  public static void generateAdditionalTransformation(ClawLanguage claw,
                                                      XcodeProgram xcodeml,
                                                      Transformer transformer,
                                                      XbaseElement stmt)
  {
    applyFusionClause(claw, xcodeml, transformer, stmt);
  }


  /**
   *
   * @param claw
   * @param xcodeml
   * @param transformer
   * @param stmt
   */
  private static void applyFusionClause(ClawLanguage claw,
                                        XcodeProgram xcodeml,
                                        Transformer transformer,
                                        XbaseElement stmt)
  {
    if(claw.hasFusionOption()){
      LoopFusion fusion = new LoopFusion((XdoStatement) stmt,
          claw.getGroupName(), claw.getPragma().getLineNo());
      transformer.addTransformation(fusion);

      if(XmOption.isDebugOutput()){
        System.out.println("Loop fusion added: " + claw.getGroupName());
      }
    }
  }

}
