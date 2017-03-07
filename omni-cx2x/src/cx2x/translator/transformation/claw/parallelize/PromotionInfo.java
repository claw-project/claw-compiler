/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.parallelize;

/**
 * Hold various information about the promotion of variable.
 *
 * @author clementval
 */
public class PromotionInfo {

  private final String _identifier;
  private final int _baseDimension;
  private final int _targetDimension;
  private final String _targetType;


  /**
   * Constructs a new PromotionInfo object with all its information.
   *
   * @param id              Identifier of the promoted variable.
   * @param baseDimension   Number of dimensions before the promotion.
   * @param targetDimension Number of dimensions after the promotion.
   * @param targetType      Type if after the promotion.
   */
  public PromotionInfo(String id, int baseDimension, int targetDimension,
                       String targetType)
  {
    _identifier = id;
    _baseDimension = baseDimension;
    _targetDimension = targetDimension;
    _targetType = targetType;
  }

  /**
   * Check whether the variable was a scalar before the promotion.
   *
   * @return True if the variable was a scalar. False otherwise.
   */
  public boolean wasScalar() {
    return _baseDimension == 0;
  }

  /**
   * Get the type id after the promotion.
   *
   * @return Type id.
   */
  public String getTargetType() {
    return _targetType;
  }


  /**
   * Get the number of dimension between the base and the target.
   *
   * @return Number of dimension.
   */
  public int diffDimension() {
    return _targetDimension - _baseDimension;
  }

}
