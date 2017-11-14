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
   * Find the dimensions defined by the one_column transformation.
   *
   * @param fctType Function type to analyze.
   * @return List of found dimensions.
   */
  public static List<DimensionDefinition> findDimensions(XfunctionType fctType,
                                                         InsertionPosition pos)
  {
    List<DimensionDefinition> dimensions = new ArrayList<>();
    if(fctType.getParams() == null) {
      return dimensions;
    }
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.getBooleanAttribute(Xattr.CLAW_INSERTED)) {
        DimensionDefinition dim = new DimensionDefinition(
            ClawConstant.ITER_PREFIX + param.value(),
            ClawConstant.DEFAULT_LOWER_BOUND,
            param.value()
        );
        dim.setInsertionPosition(pos);
        dimensions.add(dim);
      }
    }
    return dimensions;
  }

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

  /**
   * Duplicates the type to update and add extra dimensions to match the base
   * type.
   *
   * @param base       Base type.
   * @param toUpdate   Type to update.
   * @param xcodemlDst Destination XcodeML unit. Duplicate will be created here.
   * @param xcodemlSrc Source XcodeML unit. Contains base dimension.
   * @return The new type hash generated.
   */
  public static String duplicateWithDimension(XbasicType base,
                                              XbasicType toUpdate,
                                              XcodeML xcodemlDst,
                                              XcodeML xcodemlSrc,
                                              List<DimensionDefinition> dimensions)
      throws IllegalTransformationException
  {
    XbasicType newType = toUpdate.cloneNode();
    String type = xcodemlDst.getTypeTable().generateHash(XcodeType.ARRAY);
    newType.setType(type);

    if(base.isAllAssumedShape() && toUpdate.isAllAssumedShape()) {
      int additionalDimensions =
          base.getDimensions() - toUpdate.getDimensions();
      for(int i = 0; i < additionalDimensions; ++i) {
        Xnode index = xcodemlDst.createEmptyAssumedShaped();
        newType.addDimension(index, 0);
      }
    } else if(base.isAllAssumedShape() && !toUpdate.isAllAssumedShape()) {
      for(DimensionDefinition dim : dimensions) {
        switch(dim.getInsertionPosition()) {
          case BEFORE:
            // TODO control and validate the before/after
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false));
            break;
          case AFTER:
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false), 0);
            break;
          case IN_MIDDLE:
            throw new IllegalTransformationException("Not supported yet. " +
                "Insertion in middle for duplicated array type.", 0);
        }
      }
    } else {
      newType.resetDimension();

      for(int i = 0; i < base.getDimensions(); ++i) {
        Xnode newDim = xcodemlDst.createNode(Xcode.INDEXRANGE);
        newType.append(newDim);

        Xnode baseDim = base.getDimensions(i);
        Xnode lowerBound = baseDim.matchSeq(Xcode.LOWERBOUND);
        Xnode upperBound = baseDim.matchSeq(Xcode.UPPERBOUND);

        if(lowerBound != null) {
          Xnode newLowerBound =
              Range.duplicateBound(lowerBound, xcodemlSrc, xcodemlDst);
          newDim.append(newLowerBound);
        }
        if(upperBound != null) {
          Xnode newUpperBound =
              Range.duplicateBound(upperBound, xcodemlSrc, xcodemlDst);
          newDim.append(newUpperBound);
        }
        newType.addDimension(newDim);
      }

    }

    xcodemlDst.getTypeTable().add(newType);
    return type;
  }
}
