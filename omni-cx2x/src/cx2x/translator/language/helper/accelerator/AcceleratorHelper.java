/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.common.Constant;
import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.xelement.XbaseElement;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.Xpragma;

/**
 * The class AcceleratorHelper contains only static method to help the
 * generation of the accelerator related pragma during code transformations.
 *
 * @author clementval
 */
public class AcceleratorHelper {


  // OpenACC TODO will be move to specific generator when in place
  private static final String OPENACC_PARALLEL = "acc parallel";
  private static final String OPENACC_END_PARALLEL = "acc end parallel";

  /**
   * Generate corresponding pragmas to surround the code with a parallel
   * accelerated region.
   * @param claw    ClawLanguage object that tells if the parallel clause is
   *                enable and where the start pragma is located.
   * @param xcodeml Object representation of the current XcodeML representation
   *                in which the pragmas will be generated.
   * @param endStmt End statement representing the end of the parallel region.
   */
  private static void generateParallelClause(ClawLanguage claw,
                                             XcodeProgram xcodeml,
                                             XbaseElement endStmt)
  {
    if(claw.hasParallelOption()){
      // TODO depends on the target
      try {
        Xpragma beginParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        beginParallel.setValue(OPENACC_PARALLEL);
        Xpragma endParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        endParallel.setValue(OPENACC_END_PARALLEL);
        XelementHelper.insertAfter(claw.getPragma(), beginParallel);
        XelementHelper.insertAfter(endStmt, endParallel);
      } catch (IllegalTransformationException ignored) { }
    }
  }

  /**
   * Generate corresponding pragmas applied directly after a CLAW pragma.
   * @param claw    ClawLanguage object that tells if the parallel clause is
   *                enable and where the start pragma is located.
   * @param xcodeml Object representation of the current XcodeML representation
   *                in which the pragmas will be generated.
   */
  private static void generateAcceleratorClause(ClawLanguage claw,
                                                XcodeProgram xcodeml)
  {
    if(claw.hasAcceleratorOption()){
      try {
        // TODO depends on the target
        Xpragma acceleratorPragma = XelementHelper.createEmpty(Xpragma.class,
            xcodeml);
        acceleratorPragma.setValue(Constant.OPENACC_PREFIX + " " +
            claw.getAcceleratorClauses());
        XelementHelper.insertAfter(claw.getPragma(), acceleratorPragma);
      } catch (IllegalTransformationException ignored) { }
    }
  }


  /**
   * Generate all corresponding pragmas to be applied for accelerator.
   * @param claw    ClawLanguage object that tells which accelerator pragmas are
   *                enabled.
   * @param xcodeml Object representation of the current XcodeML representation
   *                in which the pragmas will be generated.
   * @param endStmt End statement for all pragma generation that need end pragma
   *                statement.
   */
  public static void generateAdditionalDirectives(ClawLanguage claw,
                                                  XcodeProgram xcodeml,
                                                  XbaseElement endStmt)
  {
    generateAcceleratorClause(claw, xcodeml);
    generateParallelClause(claw, xcodeml, endStmt);
  }




}
