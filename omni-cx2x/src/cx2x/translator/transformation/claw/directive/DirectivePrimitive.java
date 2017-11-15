/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.claw.directive;

import cx2x.translator.common.ClawConstant;
import cx2x.configuration.CompilerDirective;
import cx2x.translator.language.ClawPragma;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.translator.transformation.internal.OpenAccContinuation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.XcodeProgram;

/**
 * Directive primitive transformation allows to enable specific directive
 * primitive specified by the --directive option.
 * The "directive primitive" directive is the prefix of the directive primitive
 * language. acc for OpenACC and omp for OpenMP for example.
 *
 * @author clementval
 */
public class DirectivePrimitive extends ClawTransformation {

  /**
   * Constructs a new DirectivePrimitive triggered from a specific pragma.
   *
   * @param directive The directive that triggered the directive primitive
   *                  transformation.
   */
  public DirectivePrimitive(ClawPragma directive) {
    super(directive);
  }

  /**
   * Analysis of the transformation.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @return True always.
   */
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
    return false; // independent transformation
  }

  /**
   * Apply the directive primitive transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are
   *                       applied.
   * @param translator     The translator used to applied the transformations.
   * @param transformation Not used in this transformation
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    String prefix = CompilerDirective.getPrefix(
        _claw.getAcceleratorGenerator().getDirectiveLanguage());
    if(prefix == null) {
      return;
    }

    String regex = ClawConstant.CLAW + " *" + prefix;
    getDirective().getPragma().setValue(
        getDirective().getPragma().value().replaceAll(regex, prefix)
    );

    translator.addTransformation(xcodeml,
        new OpenAccContinuation((ClawPragma) getDirective()));
  }
}
