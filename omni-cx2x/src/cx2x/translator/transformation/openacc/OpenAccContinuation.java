/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.translator.common.Constant;
import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.Xpragma;

/**
 * <pre>
 * OpenACC line continuation transformation. The XcodeML/F prgama statement
 * representation is an aggrated version of the pragma with all its continuation
 * lines.
 * As thoses directives are not handled by the CLAW XcodeML to XcodeML
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
 * &lt;FpragmaStatement&gt;acc data acc present (a,b,c,d,e,f,g)&lt;/FpragmaStatement&gt;
 *
 * The transofrmation will split it like this:
 *
 * &lt;FpragmaStatement&gt;acc data &amp; &lt;/FpragmaStatement&gt;
 * &lt;FpragmaStatement&gt;acc present (a,b,c,d,e,f,g)&lt;/FpragmaStatement&gt;
 * </pre>
 *
 * @author clementval
 */
public class OpenAccContinuation extends Transformation<OpenAccContinuation> {

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public OpenAccContinuation(ClawLanguage directive){
    super(directive);
  }


  /**
   * Loop fusion analysis: find whether the pragma statement is followed by a
   * do statement.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return _directive.getPragma().getValue().toLowerCase().
        startsWith(Constant.OPENACC_PREFIX);
  }

  @Override
  public boolean canBeTransformedWith(OpenAccContinuation other) {
    return false; // Always false as independent transformation
  }

  /**
   * Apply the loop fusion transformation.
   * @param xcodeml         The XcodeML on which the transformations are
   *                        applied.
   * @param transformer     The transformer used to applied the transformations.
   * @param other           Not used in this transformation
   * @throws IllegalTransformationException if the transformation cannot be
   * applied.
   */
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        OpenAccContinuation other)
      throws IllegalTransformationException
  {
    String allPragma = _directive.getPragma().getValue();

    String[] pragmas = allPragma.split(Constant.OPENACC_PREFIX);

    if(pragmas.length != 2) {
      _directive.getPragma().setData(Constant.OPENACC_PREFIX + " " + pragmas[1] + " " +
          Constant.CONTINUATION_LINE_SYMBOL);
      Xpragma newlyInserted = _directive.getPragma();
      for (int i = 2; i < pragmas.length; ++i) {
        Xpragma p = XelementHelper.createEmpty(Xpragma.class, xcodeml);
        if(p == null){
          throw new IllegalTransformationException(
              "Cannot create new pragma statement"
          );
        }
        p.setFile(_directive.getPragma().getFile());
        p.setLine(_directive.getPragma().getLineNo() + (i - 1));
        if (i == pragmas.length - 1) {
          p.setData(Constant.OPENACC_PREFIX + " " + pragmas[i]);
        } else {
          p.setData(Constant.OPENACC_PREFIX + " " + pragmas[i] + " " +
              Constant.CONTINUATION_LINE_SYMBOL);
        }
        XelementHelper.insertAfter(newlyInserted, p);
        newlyInserted = p;
      }
    }
  }
}
