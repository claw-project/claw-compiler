/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

import java.util.List;

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
public class OpenAccContinuation extends ClawTransformation {


  /**
   * Constructs a new OpenACC continuation triggered from a specific pragma.
   *
   * @param directive The directive that triggered the OpenACC continuation
   *                  transformation.
   */
  public OpenAccContinuation(ClawLanguage directive) {
    super(directive);
  }


  /**
   * Check if the directive starts with the OpenACC prefix.
   *
   * @param xcodeml    The XcodeML on which the transformations are applied.
   * @param translator The translator used to applied the transformations.
   * @return True the directive starts with the OpenACC prefix.
   */
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
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
    if(isFromPrimitive()) {
      splitByCont(xcodeml);
    } else if(translator.getMaxColumns() > 0
        && !getDirective().getPragma().isDeleted())
    {
      splitByLength(xcodeml, translator);
    }
  }

  /**
   * Split the line by its length and add continuation symbols.
   *
   * @param xcodeml    The XcodeML on which the transformations are
   *                   applied.
   * @param translator The translator used to applied the transformations.
   */
  private void splitByLength(XcodeProgram xcodeml, Translator translator) {
    String allPragma = getDirective().getPragma().value().toLowerCase();
    if(allPragma.length() > translator.getMaxColumns()) {
      allPragma = XnodeUtil.dropEndingComment(allPragma);
      Xnode newlyInserted = getDirective().getPragma();
      int lineIndex = getDirective().getPragma().lineNo();
      List<String> splittedPragmas = XnodeUtil.splitByLength(allPragma,
          translator.getMaxColumns(), ClawConstant.OPENACC_PREFIX);

      for(int i = 0; i < splittedPragmas.size(); ++i) {
        // Create pragma with continuation symbol unless for the last item.
        newlyInserted = createAndInsertPragma(xcodeml, newlyInserted, lineIndex,
            splittedPragmas.get(i), i != splittedPragmas.size() - 1);
      }
      // Delete original unsplitted pragma.
      getDirective().getPragma().delete();
    }
  }

  /**
   * Split the line by its previous continuation mark.
   *
   * @param xcodeml The XcodeML on which the transformations are
   *                applied.
   */
  private void splitByCont(XcodeProgram xcodeml) {
    String allPragma = getDirective().getPragma().value();
    int lineIndex = getDirective().getPragma().lineNo();
    String splitter = ClawConstant.OPENACC_PREFIX;
    if(allPragma.contains(ClawConstant.OPENACC_PREFIX_CONT)) {
      splitter = ClawConstant.OPENACC_PREFIX_CONT;
    }

    Xnode newlyInserted = getDirective().getPragma();
    String[] lines = allPragma.split(splitter);
    for(int i = 0; i < lines.length - 1; ++i) {
      newlyInserted = createAndInsertPragma(xcodeml, newlyInserted, lineIndex++,
          lines[i], true);
    }
    createAndInsertPragma(xcodeml, newlyInserted, lineIndex,
        lines[lines.length - 1], false);
  }

  /**
   * Check if the pragma was already continued. Can happen when using the !$claw
   * primitive directive
   *
   * @return True if the pragma was previously continued.
   */
  private boolean isFromPrimitive() {
    if(getDirective().getPragma().isDeleted()) {
      return false;
    }
    String allPragma = getDirective().getPragma().value();
    return allPragma.contains(ClawConstant.OPENACC_PREFIX_CONT) ||
        Utility.countOccurrences(allPragma,
            ClawConstant.OPENACC_PREFIX + " ") > 1;
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
      if(!value.trim().toLowerCase().startsWith(ClawConstant.OPENACC_PREFIX)) {
        p.setValue(ClawConstant.OPENACC_PREFIX + " " + value.trim() + " " +
            ClawConstant.CONTINUATION_LINE_SYMBOL);
      } else {
        p.setValue(value.trim() + " " + ClawConstant.CONTINUATION_LINE_SYMBOL);
      }
    } else {
      if(!value.trim().toLowerCase().startsWith(ClawConstant.OPENACC_PREFIX)) {
        p.setValue(ClawConstant.OPENACC_PREFIX + " " + value.trim());
      }
    }
    hook.insertAfter(p);
    getDirective().getPragma().delete();
    return p;
  }

  @Override
  public boolean abortOnFailedAnalysis() {
    return false;
  }
}
