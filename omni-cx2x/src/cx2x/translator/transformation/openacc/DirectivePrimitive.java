/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
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
  public DirectivePrimitive(ClawLanguage directive) {
    super(directive);
  }

  /**
   * Analysis of the transformation.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True always.
   */
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
    // independent transformation
    return false;
  }

  /**
   * Apply the directive primitive transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are
   *                       applied.
   * @param transformer    The transformer used to applied the transformations.
   * @param transformation Not used in this transformation
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    String prefix = AcceleratorDirective.getPrefix(
        _claw.getAcceleratorGenerator().getDirectiveLanguage());
    if(prefix == null) {
      return;
    }

    String regex = ClawConstant.CLAW + " *" + prefix;
    getDirective().getPragma().setValue(
        getDirective().getPragma().value().toLowerCase().
            replaceAll(regex, prefix)
    );
  }
}
