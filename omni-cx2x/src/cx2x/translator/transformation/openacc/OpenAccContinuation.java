/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.translator.common.ClawConstant;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.language.AnalyzedPragma;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
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
public class OpenAccContinuation extends Transformation {

  /**
   * Constructs a new OpenACC continuation triggered from a specific pragma.
   *
   * @param directive The directive that triggered the OpenACC continuation
   *                  transformation.
   */
  public OpenAccContinuation(AnalyzedPragma directive) {
    super(directive);
  }


  /**
   * Check if the directive starts with the OpenACC prefix.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True the directive starts with the OpenACC prefix.
   */
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return getDirective().getPragma().value().toLowerCase().
        startsWith(ClawConstant.OPENACC_PREFIX);
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
   * Apply the OpenACC continuation transformation.
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
    if(transformer.getMaxColumns() <= 0
        || getDirective().getPragma().isDeleted())
    {
      return;
    }

    String allPragma = getDirective().getPragma().value();

    if(allPragma.length() > transformer.getMaxColumns()) {
      allPragma =
          allPragma.toLowerCase().replace(ClawConstant.OPENACC_PREFIX, "");
      Xnode newlyInserted = getDirective().getPragma();
      int lineIndex = 0;
      int addLength = ClawConstant.OPENACC_PREFIX_LENGTH + 2; // Prefix + " &"
      while(allPragma.length() > (transformer.getMaxColumns() - addLength)) {
        int splitIndex =
            allPragma.substring(0,
                transformer.getMaxColumns() - addLength).lastIndexOf(" ");
        String splittedPragma = allPragma.substring(0, splitIndex);
        allPragma = allPragma.substring(splitIndex, allPragma.length());
        newlyInserted = createAndInsertPragma(xcodeml, newlyInserted, lineIndex,
            splittedPragma, true);
      }
      createAndInsertPragma(xcodeml, newlyInserted, lineIndex,
          allPragma, false);
      getDirective().getPragma().delete();
    }
  }

  /**
   * Create a new pragma node and insert it after the hook.
   *
   * @param xcodeml   Current XcodeML file unit.
   * @param hook      Hook node. New node will be inserted after this one.
   * @param lineIndex Line index specify the offset of the line number for the
   *                  new node from the original pragma node.
   * @param value     Value of the pragma node.
   * @param continued If true, continuation symbol is added at the end of the
   *                  line.
   * @return The newly created node to be able to insert after it.
   */
  private Xnode createAndInsertPragma(XcodeProgram xcodeml, Xnode hook,
                                      int lineIndex, String value,
                                      boolean continued)
  {
    Xnode p = new Xnode(Xcode.FPRAGMASTATEMENT, xcodeml);
    p.setFilename(getDirective().getPragma().filename());
    p.setLine(getDirective().getPragma().lineNo() + lineIndex);
    if(continued) {
      p.setValue(ClawConstant.OPENACC_PREFIX + " " + value + " " +
          ClawConstant.CONTINUATION_LINE_SYMBOL);
    } else {
      p.setValue(ClawConstant.OPENACC_PREFIX + " " + value);
    }
    hook.insertAfter(p);
    getDirective().getPragma().delete();
    return p;
  }
}
