/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

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
                                             XbaseElement endStmt,
                                             AcceleratorGenerator generator)
  {
    if(claw.hasParallelClause()){
      try {
        Xpragma beginParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        beginParallel.setValue(generator.getStartParellelDirective());
        Xpragma endParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        endParallel.setValue(generator.getEndParellelDirective());
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
                                                XcodeProgram xcodeml,
                                                AcceleratorGenerator generator)
  {
    if(claw.hasAcceleratorClause())
    {
      try {
        Xpragma acceleratorPragma = XelementHelper.createEmpty(Xpragma.class,
            xcodeml);
        acceleratorPragma.setValue(
            generator.getSingleDirective(claw.getAcceleratorClauses())
        );
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
    if(claw.getAcceleratorDirective() == AcceleratorDirective.NONE){
      return;
    }

    // Get specific instance of accelerator generator
    AcceleratorGenerator generator = AcceleratorHelper.
        createAcceleratorGenerator(claw.getAcceleratorDirective());

    generateAcceleratorClause(claw, xcodeml, generator);
    generateParallelClause(claw, xcodeml, endStmt, generator);
  }

  /**
   * Constructs the correct AcceleratorGenerator object regarding the enum
   * value passed.
   * @param accDirective Enum value that define the generator to be created.
   * @return A specific implementation of an AcceleratorGenerator.
   */
  private static AcceleratorGenerator createAcceleratorGenerator(
          AcceleratorDirective accDirective)
  {
    switch (accDirective){
      case OPENACC:
        return new OpenAcc();
      case OPENMP:
        return new OpenMp();
      default:
        return new AcceleratorGenerator();
    }
  }

}
