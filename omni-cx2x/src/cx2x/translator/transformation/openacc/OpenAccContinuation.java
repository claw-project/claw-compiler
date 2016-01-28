/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.openacc;

import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProg;
import cx2x.xcodeml.xelement.Xpragma;

/**
 * @author clementval
 */
public class OpenAccContinuation extends Transformation<OpenAccContinuation> {

  public static final String OPEN_ACC_PREFIX = "acc";

  private static final String OPEN_ACC_START = "acc ";
  private static final String OPEN_ACC_CONT = " &";

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   * @param pragma The pragma that triggered the loop fusion transformation.
   */
  public OpenAccContinuation(Xpragma pragma){
    super(pragma);
  }

  /**
   * Loop fusion analysis: find whether the pragma statement is followed by a
   * do statement.
   * @param xcodeml      The XcodeML on which the transformations are applied.
   * @param transformer  The transformer used to applied the transformations.
   * @return True if a do statement is found. False otherwise.
   */
  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    return _pragma.getData().toLowerCase().startsWith(OPEN_ACC_PREFIX);
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
   * @throws IllegalTransformationException
   */
  public void transform(XcodeProg xcodeml, Transformer transformer,
                        OpenAccContinuation other)
      throws IllegalTransformationException
  {
    String allPragma = _pragma.getData();



    String[] pragmas = allPragma.split("acc");

    if(pragmas.length != 2) {
      _pragma.setData(OPEN_ACC_START + pragmas[1] + OPEN_ACC_CONT);
      Xpragma newlyInserted = _pragma;
      for (int i = 2; i < pragmas.length; ++i) {
        Xpragma p = Xpragma.createEmpty(xcodeml);
        p.setFilename(_pragma.getFilename());
        p.setLine(_pragma.getLine() + (i - 1));
        if (i == pragmas.length - 1) {
          p.setData(OPEN_ACC_START + pragmas[i]);
        } else {
          p.setData(OPEN_ACC_START + pragmas[i] + OPEN_ACC_CONT);
        }
        XelementHelper.insertAfter(newlyInserted, p);
        newlyInserted = p;
      }
    }
  }
}
