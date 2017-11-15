/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.language.helper;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.transformation.primitive.Range;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.language.InsertionPosition;
import cx2x.xcodeml.xnode.*;

import java.util.ArrayList;
import java.util.List;

/**
 * The class TransformationHelper contains only static method to help the
 * generation of additional transformation described as clause in main
 * directive.
 *
 * @author clementval
 */
public class TransformationHelper {

  // TODO 1.0 move method to specific primitives or classes and delete this class

  /**
   * Get the number of base dimension in an over clause.
   *
   * @param over Over clause as a list of string element.
   * @return The number of base dimension.
   */
  public static int baseDimensionNb(List<String> over) {
    int cnt = 0;
    for(String dim : over) {
      if(dim.equals(DimensionDefinition.BASE_DIM)) {
        ++cnt;
      }
    }
    return cnt;
  }
}
