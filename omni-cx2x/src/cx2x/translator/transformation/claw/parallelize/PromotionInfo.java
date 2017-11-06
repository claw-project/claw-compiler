/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.claw.parallelize;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.OverPosition;
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
  private OverPosition _overPosition = OverPosition.BEFORE; // Default
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
   * Constructs a new PromotionInfo object with an identifier and information
   * extracted from the ClawLanguage directive.
   *
   * @param id   Field identifier.
   * @param claw Current CLAW language directive information.
   */
  public PromotionInfo(String id, ClawLanguage claw) {
    this(id, claw, 0);
  }

  /**
   * Constructs a new PromotionInfo object with an identifier and information
   * extracted from the ClawLanguage directive.
   *
   * @param id        Field identifier.
   * @param claw      Current CLAW language directive information.
   * @param overIndex Over clause to be used. Index starts at 0.
   */
  public PromotionInfo(String id, ClawLanguage claw, int overIndex) {
    _identifier = id.toLowerCase();
    _dimensions = claw.getDimensionValues();

    if(claw.hasOverClause()) {
      _overPosition =
          OverPosition.fromList(claw.getOverClauseValues().get(overIndex));
    }
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

  public void setBaseDimension(int value) {
    _baseDimension = value;
  }

  public void setTargetDimension(int value) {
    _targetDimension = value;
  }

  public List<DimensionDefinition> getDimensions() {
    return _dimensions;
  }

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
   * Get associated OverPosition.
   *
   * @return OverPosition value.
   */
  public OverPosition getOverPosition() {
    return _overPosition;
  }

  /**
   * Set the OverPosition value.
   *
   * @param overPosition New OverPosition value.
   */
  public void setOverPosition(OverPosition overPosition) {
    _overPosition = overPosition;
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

  public void setTargetType(String value) {
    _targetType = value;
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

  public enum PromotionType {SCALAR_TO_ARRAY, ARRAY_TO_ARRAY}

}
