/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.translator.transformation.loop.LoopInterchange;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XbaseElement;
import cx2x.xcodeml.xelement.XdoStatement;
import xcodeml.util.XmOption;

/**
 * The class TransformationHelper contains only static method to help the
 * generation of additional transformation described as clause in main
 * directive.
 *
 * @author clementval
 */
public class TransformationHelper {


  /**
   * Generate corresponding additional transformation according to optional
   * clauses given to the directive.
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached.
   */
  public static void generateAdditionalTransformation(ClawLanguage claw,
                                                      Transformer transformer,
                                                      XbaseElement stmt)
  {
    // Order doesn't matter
    applyFusionClause(claw, transformer, stmt);
    applyInterchangeClause(claw, transformer, stmt);
  }


  /**
   * Generate loop fusion transformation if the clause is present in the
   * directive.
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached. Must
   *                    be a FdoStatement for the loop fusion transformation.
   */
  private static void applyFusionClause(ClawLanguage claw,
                                        Transformer transformer,
                                        XbaseElement stmt)
  {
    if(claw.hasFusionClause() && stmt instanceof XdoStatement){
      ClawLanguage l = ClawLanguage.createLoopFusionLanguage(claw);
      LoopFusion fusion = new LoopFusion((XdoStatement) stmt, l);
      // TODO maybe run analysis
      transformer.addTransformation(fusion);

      if(XmOption.isDebugOutput()){
        System.out.println("Loop fusion added: " + claw.getGroupValue());
      }
    }
  }

  /**
   * Generate loop interchange transformation if the clause is present in the
   * directive.
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached. Must
   *                    be a FdoStatement for the loop interchange
   *                    transformation.
   */
  private static void applyInterchangeClause(ClawLanguage claw,
                                             Transformer transformer,
                                             XbaseElement stmt)
  {
    if(claw.hasInterchangeClause() && stmt instanceof XdoStatement){
      // TODO
      LoopInterchange interchange = new LoopInterchange(claw);

      transformer.addTransformation(interchange);
      if(XmOption.isDebugOutput()){
        System.out.println("Loop interchange added: " + claw.getIndexes());
      }
    }
  }

}
