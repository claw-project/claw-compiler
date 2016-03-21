/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XcodeProgram;

/**
 * <pre>
 * An ArrayTranform transformation is an independent transformation. It
 * transforms the Fortran array notation into single or nested do statements.
 *
 * Array notation example:
 * A(1:n) = A(1+m:n+m) + B(1:n) * C(n+1:n+n)
 *
 * DO i=1,n
 *   A(i) = A(i+m) + B(i) * C(n+i)
 * END DO
 * </PRE>
 *
 * @author clementval
 */
public class ArrayTransform extends Transformation<ArrayTransform> {

  /**
   * Constructs a new ArrayTransform triggered from a specific directive.
   * @param directive The directive that triggered the array transform
   *                  transformation.
   */
  public ArrayTransform(ClawLanguage directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    return true;
  }

  @Override
  public boolean canBeTransformedWith(ArrayTransform other) {
    return false;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        ArrayTransform other) throws Exception
  {

  }
}
