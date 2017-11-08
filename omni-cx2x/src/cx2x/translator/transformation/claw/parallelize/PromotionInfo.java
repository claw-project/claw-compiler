/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.parallelize;

import cx2x.xcodeml.language.DimensionDefinition;

import java.util.List;

/**
 * Hold various information about the promotion of a variable.
 *
 * @author clementval
 */
public class PromotionInfo {

  private final String _identifier;
  private int _baseDimension;
  private int _targetDimension;
  private String _targetType;
  private List<DimensionDefinition> _dimensions;
  private PromotionType _promotionType = PromotionType.ARRAY_TO_ARRAY; //Default

  /**
   * Constructs a new PromotionInfo object with the identifier of the
   * field associated with this promotion object.
   *
   * @param id Field identifier.
   */
  public PromotionInfo(String id) {
    _identifier = id.toLowerCase();
  }

  /**
   * Constructs a new PromotionInfo object with the identifier of the field
   * associated with this promotion and the DimensionDefiniton used for
   * promotion.
   *
   * @param id         Field identifier.
   * @param dimensions List of dimension definitions to use for the promotion.
   */
  public PromotionInfo(String id, List<DimensionDefinition> dimensions) {
    _identifier = id;
    _dimensions = dimensions;
  }

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
   * Get the list of dimension definition used for promotion.
   *
   * @return List of dimension definitions.
   */
  public List<DimensionDefinition> getDimensions() {
    return _dimensions;
  }

  /**
   * Set the list of dimension definitions to be used.
   *
   * @param dimensions List of dimension definitions.
   */
  public void setDimensions(List<DimensionDefinition> dimensions) {
    _dimensions = dimensions;
  }

  /**
   * Get the associated identifier.
   *
   * @return Identifier.
   */
  public String getIdentifier() {
    return _identifier;
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
   * Set the value of the type after promotion.
   *
   * @param value Type hash value.
   */
  public void setTargetType(String value) {
    _targetType = value;
  }

  /**
   * Get the number of target dimension. After promotion.
   *
   * @return Number of dimension after promotion.
   */
  public int getTargetDimension() {
    return _targetDimension;
  }

  /**
   * Set the value of the target dimension.
   *
   * @param value int value.
   */
  public void setTargetDimension(int value) {
    _targetDimension = value;
  }

  /**
   * Get the number of base dimension. Before promotion.
   *
   * @return Number of dimension before promotion.
   */
  public int getBaseDimension() {
    return _baseDimension;
  }

  /**
   * Set the value of the base dimension number.
   *
   * @param value New value for the base dimension number.
   */
  public void setBaseDimension(int value) {
    _baseDimension = value;
  }

  /**
   * Get the number of dimension between the base and the target.
   *
   * @return Number of dimension.
   */
  public int diffDimension() {
    return _targetDimension - _baseDimension;
  }

  /**
   * Get the promotion type.
   *
   * @return Current promotion type value.
   */
  public PromotionType getPromotionType() {
    return _promotionType;
  }

  /**
   * Set the promotion type value.
   *
   * @param promotionType New promotion type value.
   */
  public void setPromotionType(PromotionType promotionType) {
    _promotionType = promotionType;
  }

  // Type of promotion
  public enum PromotionType {
    SCALAR_TO_ARRAY, ARRAY_TO_ARRAY
  }

}
