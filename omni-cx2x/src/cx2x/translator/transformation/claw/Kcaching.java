/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw;

import cx2x.xcodeml.exception.IllegalDirectiveException;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.Xpragma;

/**
 * A Kcaching transformation is an independent transformation. The
 * transformation consists of placing an assignment in a scalar variable and
 * use this variable in a loop body before updating it.
 *
 * @author clementval
 */
public class Kcaching extends Transformation<Kcaching> {

  /**
   * Constructs a new Kcachine triggered from a specific pragma.
   * @param pragma  The pragma that triggered the k caching transformation.
   * @throws IllegalDirectiveException if something is wrong in the directive's
   * options
   */
  public Kcaching(Xpragma pragma) throws IllegalDirectiveException {
    super(pragma);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return false;
  }

  @Override
  public boolean canBeTransformedWith(Kcaching other) {
    return false;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Kcaching other) throws Exception
  {

  }
}
