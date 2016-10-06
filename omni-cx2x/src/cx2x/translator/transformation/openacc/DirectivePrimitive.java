/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

/**
 * <pre>
 * OpenACC line continuation transformation. The XcodeML/F pragma statement
 * representation is an aggregated version of the pragma with all its continuation
 * lines.
 * As those directives are not handled by the CLAW XcodeML to XcodeML
 * translator, they must be output in a correct way. This transformation divides
 * the XcodeML representation back to a multi-line pragma definition.
 *
 * Example:
 * The followings OpenACC directives in Fortran code:
 *
 *   !$acc data &amp;
 *   !$acc present (a,b,c,d,e,f,g)
 *
 * are represented in XcodeML with
 *
 * &lt;FpragmaStatement&gt;acc data present (a,b,c,d,e,f,g)&lt;/FpragmaStatement&gt;
 *
 * Based on the defined max columns, the pragma statement will be splitted.
 * </pre>
 *
 * @author clementval
 */
public class DirectivePrimitive extends Transformation {

  private final ClawLanguage _claw;

  /**
   * Constructs a new DirectivePrimitive triggered from a specific pragma.
   * @param directive The directive that triggered the directive primitive
   *                  transformation.
   */
  public DirectivePrimitive(ClawLanguage directive){
    super(directive);
    _claw = directive;
  }

  /**
   * Analysis of the transformation.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True always.
   */
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(Transformation other) {
    // independent transformation
    return false;
  }

  /**
   * Apply the directive primitive transformation.
   * @param xcodeml         The XcodeML on which the transformations are
   *                        applied.
   * @param transformer     The transformer used to applied the transformations.
   * @param transformation  Not used in this transformation
   * @throws IllegalTransformationException if the transformation cannot be
   * applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    String prefix = AcceleratorDirective.getPrefix(
        _claw.getAcceleratorGenerator().getDirectiveLanguage());

    String regex = ClawConstant.CLAW + " *" + prefix;
    getDirective().getPragma().setValue(
        getDirective().getPragma().getValue().toLowerCase().
            replaceAll(regex, prefix)
    );
  }
}
