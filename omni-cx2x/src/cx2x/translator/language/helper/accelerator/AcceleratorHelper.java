/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper.accelerator;

import cx2x.translator.language.ClawLanguage;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformer;
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
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param endStmt   End statement representing the end of the parallel region.
   * @param generator An instance of accelerator generator.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  private static XbaseElement generateParallelClause(
      ClawLanguage claw, XcodeProgram xcodeml, XbaseElement startStmt,
      XbaseElement endStmt, AcceleratorGenerator generator)
  {
    if(claw.hasParallelClause()){
      try {
        Xpragma beginParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        beginParallel.setValue(generator.getStartParellelDirective());
        Xpragma endParallel =
            XelementHelper.createEmpty(Xpragma.class, xcodeml);
        endParallel.setValue(generator.getEndParellelDirective());
        XelementHelper.insertBefore(startStmt, beginParallel);
        XelementHelper.insertAfter(endStmt, endParallel);
        return endParallel;
      } catch (IllegalTransformationException ignored) { }
    }
    return null;
  }

  /**
   * Generate corresponding pragmas applied directly after a CLAW pragma.
   * @param claw      ClawLanguage object that tells if the parallel clause is
   *                  enable and where the start pragma is located.
   * @param startStmt Start statement representing the beginning of the parallel
   *                  region.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param generator An instance of accelerator generator.
   * @return Last stmt inserted or null if nothing is inserted.
   */
  private static XbaseElement generateAcceleratorClause(
      ClawLanguage claw, XcodeProgram xcodeml, XbaseElement startStmt,
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
        XelementHelper.insertBefore(startStmt, acceleratorPragma);
        return acceleratorPragma;
      } catch (IllegalTransformationException ignored) { }
    }
    return null;
  }

  /**
   * Generate all corresponding pragmas to be applied for accelerator.
   * @param claw      ClawLanguage object that tells which accelerator pragmas
   *                  are enabled.
   * @param xcodeml   Object representation of the current XcodeML
   *                  representation in which the pragmas will be generated.
   * @param startStmt Start statement for all pragma generation.
   * @param endStmt   End statement for all pragma generation that need end
   *                  pragma statement.
   * @return Last stmt inserted.
   */
  public static XbaseElement generateAdditionalDirectives(
      ClawLanguage claw, XcodeProgram xcodeml, XbaseElement startStmt,
      XbaseElement endStmt)
  {
    if(claw.getAcceleratorDirective() == AcceleratorDirective.NONE){
      return null;
    }

    // Get specific instance of accelerator generator
    AcceleratorGenerator generator = AcceleratorHelper.
        createAcceleratorGenerator(claw.getAcceleratorDirective());

    XbaseElement pragma =
        generateAcceleratorClause(claw, xcodeml, startStmt, generator);
    if(pragma != null){
      startStmt = pragma;
    }
    return generateParallelClause(claw, xcodeml, startStmt, endStmt, generator);
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
                                           Transformer transformer,
                                           XbaseElement stmt,
                                           String var)
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
      if(parallel.getValue().length() >= 80){
        parallel.append(" " + generator.getPrefix() + " ");
        transformer.addTransformation(new OpenAccContinuation(
            new AnalyzedPragma(parallel)));
      }
      parallel.append(generator.getPrivateClause(var));
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
