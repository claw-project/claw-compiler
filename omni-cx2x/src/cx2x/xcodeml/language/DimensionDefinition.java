/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.language;

import cx2x.xcodeml.xnode.*;

/**
 * Class holding information about defined dimension.
 *
 * @author clementval
 */
public class DimensionDefinition {

  public static final String BASE_DIM = ":";
  private static final int DEFAULT_STEP_VALUE = 1;

  private final BoundDefinition _lowerBound;
  private final BoundDefinition _upperBound;
  private final String _identifier; // Used as array index
  private InsertionPosition _insertionPosition = InsertionPosition.BEFORE;

  private DimensionDefinition(String id, BoundDefinition lowerBound,
                              BoundDefinition upperBound)
  {
    _identifier = id;
    _lowerBound = lowerBound;
    _upperBound = upperBound;
  }

  /**
   * Constructs a new dimension object from the extracted information.
   *
   * @param id         Identifier of the defined dimension. Will be used as the
   *                   array index variable.
   * @param lowerBound Lower bound of the dimension.
   * @param upperBound Upper bound of the dimension.
   *                   TODO maybe add step information (in the grammar as well)
   */
  public DimensionDefinition(String id, String lowerBound, String upperBound) {
    _identifier = id;
    _lowerBound =
        new BoundDefinition(lowerBound, BoundDefinition.BoundType.LOWER);
    _upperBound =
        new BoundDefinition(upperBound, BoundDefinition.BoundType.UPPER);
  }

  /**
   * Get lower bound definition.
   *
   * @return lower bound definition.
   */
  public BoundDefinition getLowerBound() {
    return _lowerBound;
  }

  /**
   * Get upper bound definition.
   *
   * @return upper bound definition.
   */
  public BoundDefinition getUpperBound() {
    return _upperBound;
  }

  /**
   * Get the identifier for the current dimension.
   *
   * @return The identifier of the current dimension.
   */
  public String getIdentifier() {
    return _identifier;
  }

  /**
   * @return
   */
  public InsertionPosition getInsertionPosition() {
    return _insertionPosition;
  }

  /**
   * @param position
   */
  public void setInsertionPosition(InsertionPosition position) {
    _insertionPosition = position;
  }

  /**
   * Generate the correct indexRange element with lowerBound, upperBound and
   * step from the current dimension.
   *
   * @param xcodeml  Current XcodeML program unit in which elements will be
   *                 created.
   * @param withStep IF true, step element is created.
   * @return A new indexRange elements.
   */
  public Xnode generateIndexRange(XcodeML xcodeml, boolean withStep) {
    Xnode range = xcodeml.createNode(Xcode.INDEXRANGE);
    range.append(_lowerBound.generate(xcodeml));
    range.append(_upperBound.generate(xcodeml));
    if(withStep) {
      Xnode step = xcodeml.createNode(Xcode.STEP);
      step.append(xcodeml.createIntConstant(DEFAULT_STEP_VALUE));
      range.append(step);
    }
    return range;
  }

  /**
   * Generate the array index that will be placed in the array reference for
   * this additional dimension.
   *
   * @param xcodeml Current XcodeML program unit.
   * @return A new arrayIndex element including a var element with the dimension
   * identifier.
   */
  public Xnode generateArrayIndex(XcodeML xcodeml) {
    Xnode aIdx = xcodeml.createNode(Xcode.ARRAYINDEX);
    aIdx.append(xcodeml.createVar(XbuiltInType.INT, _identifier, Xscope.LOCAL));
    return aIdx;
  }

  /**
   * Generate additional node to be inserted in the allocate statement
   * arrayIndex based on the dimension definition.
   *
   * @param xcodeml Current XcodeML program unit.
   * @return New node to be inserted in arrayIndex of allocate statement.
   */
  public Xnode generateAllocateNode(XcodeProgram xcodeml) {
    // TODO handle special size with lowerBound != 1
    Xnode arrayIndex = xcodeml.createNode(Xcode.ARRAYINDEX);
    arrayIndex.append(_upperBound.generateValueNode(xcodeml));
    return arrayIndex;
  }

  public DimensionDefinition copy() {
    return new DimensionDefinition(_identifier, _lowerBound, _upperBound);
  }
}
