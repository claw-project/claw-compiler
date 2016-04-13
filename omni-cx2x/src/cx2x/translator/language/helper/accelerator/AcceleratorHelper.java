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

import java.util.List;

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
   * Generate the correct clauses for private variable on accelerator.
   * @param claw    ClawLanguage object that tells which accelerator pragmas
   *                are enabled.
   * @param xcodeml Object representation of the current XcodeML
   *                representation in which the pragmas will be generated.
   * @param stmt    Statement from which we looks for a parallel clause to
   *                append private clauses.
   * @param vars    List of variables for the private clause.
   * TODO what about OpenMP
   */
  public static void generatePrivateClause(ClawLanguage claw,
                                           XcodeProgram xcodeml,
                                           XbaseElement stmt,
                                           List<String> vars)
  {
    if(claw.getAcceleratorDirective() == AcceleratorDirective.NONE
        || !claw.hasPrivateClause()){
      return;
    }

    AcceleratorGenerator generator = AcceleratorHelper.
        createAcceleratorGenerator(claw.getAcceleratorDirective());

    Xpragma parallel =
        XelementHelper.findPreviousPragma(stmt, generator.getParallelKeyword());

    if(parallel == null){
      xcodeml.addWarning("No parallel construct found to attach private clause",
          claw.getPragma().getLineNo());
    } else {
      for (String var : vars) {
        parallel.append(generator.getPrivateClause(var));
      }
    }
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
    }
    return null;
  }

}
