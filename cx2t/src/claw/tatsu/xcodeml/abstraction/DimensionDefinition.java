/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;

import java.util.Collections;
import java.util.List;

/**
 * Class holding information about defined dimension.
 *
 * @author clementval
 */
public class DimensionDefinition {

  public static final DimensionDefinition BASE_DIMENSION
      = createBaseDimension();

  public static final String SEPARATOR = ",";
  public static final String BASE_DIM = ":";
  private static final String BASE_DIMENSION_ID = "__BASE__";
  private static final int DEFAULT_STEP_VALUE = 1;

  private final BoundDefinition _lowerBound;
  private final BoundDefinition _upperBound;

  private final BoundDefinition _iterationLowerBound;
  private final BoundDefinition _iterationUpperBound;
  private final BoundDefinition _iterationStep;

  private final String _identifier; // Used as array index
  private InsertionPosition _insertionPosition = InsertionPosition.BEFORE;

  /**
   * Create the special dimension definition used as place holder for
   * currently existing dimensions.
   *
   * @return A DimensionDefinition object with __BASE__ as id and undefined
   * bounds.
   */
  private static DimensionDefinition createBaseDimension() {
    return new DimensionDefinition(BASE_DIMENSION_ID,
        new BoundDefinition("", BoundDefinition.BoundType.LOWER),
        new BoundDefinition("", BoundDefinition.BoundType.UPPER));
  }

  /**
   * Internal constructor with bound object.
   *
   * @param id         Identifier of the defined dimension. Will be used as the
   *                   array index variable.
   * @param lowerBound Lower bound of the dimension.
   * @param upperBound Upper bound of the dimension.
   */
  private DimensionDefinition(String id, BoundDefinition lowerBound,
                              BoundDefinition upperBound)
  {
    _identifier = id;
    _lowerBound = lowerBound;
    _upperBound = upperBound;
    _iterationLowerBound = lowerBound;
    _iterationUpperBound = upperBound;
    _iterationStep = new BoundDefinition(String.valueOf(DEFAULT_STEP_VALUE),
        BoundDefinition.BoundType.STEP);
  }

  /**
   * Constructs a new dimension object from the extracted information.
   *
   * @param id         Identifier of the defined dimension. Will be used as the
   *                   array index variable.
   * @param lowerBound Lower bound of the dimension.
   * @param upperBound Upper bound of the dimension.
   */
  public DimensionDefinition(String id, String lowerBound, String upperBound) {
    _identifier = id;
    _lowerBound =
        new BoundDefinition(lowerBound, BoundDefinition.BoundType.LOWER);
    _upperBound =
        new BoundDefinition(upperBound, BoundDefinition.BoundType.UPPER);
    _iterationLowerBound = _lowerBound;
    _iterationUpperBound = _upperBound;
    _iterationStep = new BoundDefinition(String.valueOf(DEFAULT_STEP_VALUE),
        BoundDefinition.BoundType.STEP);
  }

  /**
   * Constructs a new dimension object from the extracted bound and iteration
   * information.
   *
   * @param id           Identifier of the defined dimension. Will be used as
   *                     the array index variable.
   * @param lowerBound   Lower bound of the dimension.
   * @param upperBound   Upper bound of the dimension.
   * @param itLowerBound Iteration lower bound. If null, lower bound information
   *                     is the default.
   * @param itUpperBound Iteration upper bound. If null, upper bound information
   *                     is the default.
   * @param step         Iteration step. If null, 1 is the default
   */
  public DimensionDefinition(String id, String lowerBound, String upperBound,
                             String itLowerBound, String itUpperBound,
                             String step)
  {
    _identifier = id;
    _lowerBound =
        new BoundDefinition(lowerBound, BoundDefinition.BoundType.LOWER);
    _upperBound =
        new BoundDefinition(upperBound, BoundDefinition.BoundType.UPPER);

    if(itLowerBound != null) {
      _iterationLowerBound =
          new BoundDefinition(itLowerBound, BoundDefinition.BoundType.LOWER);
    } else {
      _iterationLowerBound = _lowerBound;
    }

    if(itUpperBound != null) {
      _iterationUpperBound =
          new BoundDefinition(itUpperBound, BoundDefinition.BoundType.UPPER);
    } else {
      _iterationUpperBound = _upperBound;
    }

    if(step != null) {
      _iterationStep =
          new BoundDefinition(step, BoundDefinition.BoundType.STEP);
    } else {
      _iterationStep = new BoundDefinition(String.valueOf(DEFAULT_STEP_VALUE),
          BoundDefinition.BoundType.STEP);
    }
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
   * Get iteration lower bound definition.
   *
   * @return iteration lower bound definition.
   */
  public BoundDefinition getIterationLowerBound() {
    return _iterationLowerBound;
  }

  /**
   * Get iteration upper bound definition.
   *
   * @return iteration upper bound definition.
   */
  public BoundDefinition getIterationUpperBound() {
    return _iterationUpperBound;
  }

  /**
   * Get iteration step definition.
   *
   * @return iteration step.
   */
  public BoundDefinition getIterationStep() {
    return _iterationStep;
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
   * Get the insertion position value for this dimension definition.
   *
   * @return InsertionPosition enum value.
   */
  public InsertionPosition getInsertionPosition() {
    return _insertionPosition;
  }

  /**
   * Set the insertion position value for this dimension definition.
   *
   * @param position InsertionPosition enum value.
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
   * @param useIteration IF true, use the iterations bounds instead of the size bounds.
   * @return A new indexRange elements.
   */
  public Xnode generateIndexRange(XcodeML xcodeml, boolean withStep, boolean useIteration) {
    Xnode range = xcodeml.createNode(Xcode.INDEX_RANGE);
    if(useIteration) {
      range.append(_iterationLowerBound.generate(xcodeml));
      range.append(_iterationUpperBound.generate(xcodeml));
    } else {
      range.append(_lowerBound.generate(xcodeml));
      range.append(_upperBound.generate(xcodeml));
    }
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
    Xnode aIdx = xcodeml.createNode(Xcode.ARRAY_INDEX);
    aIdx.append(xcodeml.createVar(FortranType.INTEGER,
        _identifier, Xscope.LOCAL));
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
    Xnode arrayIndex = xcodeml.createNode(Xcode.ARRAY_INDEX);
    arrayIndex.append(_upperBound.generateValueNode(xcodeml));
    return arrayIndex;
  }

  /**
   * Create a new copy of the current dimension definition.
   *
   * @return A newly created copy of this dimension definition.
   */
  public DimensionDefinition copy() {
    return new DimensionDefinition(_identifier, _lowerBound, _upperBound);
  }

  @Override
  public String toString() {
    return String.format("%s(%s:%s)", _identifier,
        _lowerBound.isVar() ?
            _lowerBound.getValue() : String.valueOf(_lowerBound.getIntValue()),
        _upperBound.isVar() ?
            _upperBound.getValue() : String.valueOf(_upperBound.getIntValue()));
  }

  /**
   * Flag the insertion position in a list of dimension definition based on the
   * location of the base dimension.
   *
   * @param dimensions List of dimension definition to be flagged.
   */
  public static void flagInsertPosition(List<DimensionDefinition> dimensions) {
    int baseDimensionNb =
        Collections.frequency(dimensions, DimensionDefinition.BASE_DIMENSION);

    boolean hasMiddleInsertion = baseDimensionNb > 1;
    InsertionPosition crtPos = InsertionPosition.BEFORE;
    for(DimensionDefinition dim : dimensions) {
      if(dim == DimensionDefinition.BASE_DIMENSION) {
        crtPos = crtPos.getNext(hasMiddleInsertion);
      } else {
        dim.setInsertionPosition(crtPos);
      }
    }
  }
}
