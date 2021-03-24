/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import claw.tatsu.xcodeml.xnode.fortran.FbasicType;

import java.util.ArrayList;
import java.util.List;

/**
 * Hold various information about the promotion of a variable.
 *
 * @author clementval
 */
public class PromotionInfo
{

    private String _identifier;
    private int _baseDimension;
    private int _targetDimension;
    private FbasicType _targetType;
    private List<DimensionDefinition> _dimensions;
    private PromotionType _promotionType = PromotionType.ARRAY_TO_ARRAY; // Default
    private boolean _referenceAdapted = false;
    private boolean _allocateAdapted = false;
    private boolean _forceAssumendShape = false;

    /**
     * Default ctor. Used for global promotion information not attached to a field.
     */
    public PromotionInfo()
    {
    }

    /**
     * Constructs a new PromotionInfo object with the identifier of the field
     * associated with this promotion object.
     *
     * @param id Field identifier.
     */
    public PromotionInfo(String id)
    {
        _identifier = id.toLowerCase();
    }

    /**
     * Constructs a new PromotionInfo object with the identifier of the field
     * associated with this promotion and the dimension definition used for
     * promotion.
     *
     * @param id         Field identifier.
     * @param dimensions List of dimension definitions to use for the promotion.
     */
    public PromotionInfo(String id, List<DimensionDefinition> dimensions)
    {
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
    public PromotionInfo(String id, int baseDimension, int targetDimension, FbasicType targetType)
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
    public List<DimensionDefinition> getDimensions()
    {
        return _dimensions;
    }

    /**
     * Set the list of dimension definitions to be used.
     *
     * @param dimensions List of dimension definitions.
     */
    public void setDimensions(List<DimensionDefinition> dimensions)
    {
        _dimensions = dimensions;
    }

    /**
     * Get the associated identifier.
     *
     * @return Identifier.
     */
    public String getIdentifier()
    {
        return _identifier;
    }

    /**
     * Check whether the variable was a scalar before the promotion.
     *
     * @return True if the variable was a scalar. False otherwise.
     */
    public boolean wasScalar()
    {
        return _baseDimension == 0;
    }

    /**
     * Get the type id after the promotion.
     *
     * @return Type id.
     */
    public FbasicType getTargetType()
    {
        return _targetType;
    }

    /**
     * Set the value of the type after promotion.
     *
     * @param value Type hash value.
     */
    public void setTargetType(FbasicType value)
    {
        _targetType = value;
    }

    /**
     * Get the number of target dimension. After promotion.
     *
     * @return Number of dimension after promotion.
     */
    public int getTargetDimension()
    {
        return _targetDimension;
    }

    /**
     * Set the value of the target dimension.
     *
     * @param value int value.
     */
    public void setTargetDimension(int value)
    {
        _targetDimension = value;
    }

    /**
     * Get the number of base dimension. Before promotion.
     *
     * @return Number of dimension before promotion.
     */
    public int getBaseDimension()
    {
        return _baseDimension;
    }

    /**
     * Set the value of the base dimension number.
     *
     * @param value New value for the base dimension number.
     */
    public void setBaseDimension(int value)
    {
        _baseDimension = value;
    }

    /**
     * Get the number of dimension between the base and the target.
     *
     * @return Number of dimension.
     */
    public int diffDimension()
    {
        return _targetDimension - _baseDimension;
    }

    /**
     * Get the promotion type.
     *
     * @return Current promotion type value.
     */
    public PromotionType getPromotionType()
    {
        return _promotionType;
    }

    /**
     * Set the promotion type value.
     *
     * @param promotionType New promotion type value.
     */
    public void setPromotionType(PromotionType promotionType)
    {
        _promotionType = promotionType;
    }

    /**
     * Get a formatted String representing the dimensions used in this promotion
     * information object. String is formatted as follows:
     *
     * - Dimension: dimension_id(lower_bound:upper_bound)
     *
     * - Existing dimensions are represented with :
     *
     * So a promotion information with 1 additional dimension will be formatted
     * like: - New dimension before: "dimension_id(lower_bound:upper_bound),:" - New
     * dimension in middle: ":,dimension_id(lower_bound:upper_bound),:" - New
     * dimension after: ":,dimension_id(lower_bound:upper_bound)"
     *
     * @return Dimensions information in a formatted string.
     */
    public String getFormattedDimensions()
    {
        if (_dimensions == null || _dimensions.isEmpty())
        {
            return "";
        }
        StringBuilder str = new StringBuilder();
        InsertionPosition crtPos = InsertionPosition.BEFORE;
        for (int i = 0; i < _dimensions.size(); ++i)
        {
            DimensionDefinition dim = _dimensions.get(i);
            if (dim.getInsertionPosition() != crtPos)
            {
                str.append(DimensionDefinition.BASE_DIM);
                str.append(DimensionDefinition.SEPARATOR);
                crtPos = dim.getInsertionPosition();
            }
            str.append(dim.toString());
            if (i != _dimensions.size() - 1)
            {
                str.append(DimensionDefinition.SEPARATOR);
            }
        }
        if (crtPos != InsertionPosition.AFTER)
        {
            str.append(DimensionDefinition.SEPARATOR);
            str.append(DimensionDefinition.BASE_DIM);
        }
        return str.toString();
    }

    /**
     * Check if the references associated with this promotion have been adapted
     * already.
     *
     * @return True if the references were adapted. False otherwise.
     */
    public boolean isRefAdapted()
    {
        return _referenceAdapted;
    }

    /**
     * Flag that the references associated with this promotion have been adapted.
     */
    public void setRefAdapted()
    {
        _referenceAdapted = true;
    }

    /**
     * Check if the allocate statements associated with this promotion have been
     * adapted already.
     *
     * @return True if the references were adapted. False otherwise.
     */
    public boolean isAllocateAdapted()
    {
        return _allocateAdapted;
    }

    /**
     * Flag that the allocate statements associated with this promotion have been
     * adapted.
     */
    public void setAllocateAdapted()
    {
        _allocateAdapted = true;
    }

    /**
     * Reset adapted flags.
     */
    public void resetFlags()
    {
        _referenceAdapted = false;
        _allocateAdapted = false;
    }

    /**
     * Read the dimension information from a raw string.
     *
     * @param rawValue Raw string representation of the dimension information.
     */
    public void readDimensionsFromString(String rawValue)
    {
        _dimensions = new ArrayList<>();
        String[] rawDimensions = rawValue.split(DimensionDefinition.SEPARATOR);

        int baseDimOccurrence = 0;
        for (String d : rawDimensions)
        {
            if (d.equals(DimensionDefinition.BASE_DIM))
            {
                ++baseDimOccurrence;
            }
        }
        boolean hasMiddleInsertion = baseDimOccurrence > 1;
        InsertionPosition crtPos = InsertionPosition.BEFORE;
        for (String rawDim : rawDimensions)
        {
            if (rawDim.equals(DimensionDefinition.BASE_DIM))
            {
                crtPos = crtPos.getNext(hasMiddleInsertion);
            } else
            {
                String dimensionId = rawDim.substring(0, rawDim.indexOf('('));
                String lowerBound = rawDim.substring(rawDim.indexOf('(') + 1, rawDim.indexOf(':'));
                String upperBound = rawDim.substring(rawDim.indexOf(':') + 1, rawDim.indexOf(')'));
                DimensionDefinition dim = new DimensionDefinition(dimensionId, lowerBound, upperBound);
                dim.setInsertionPosition(crtPos);
                _dimensions.add(dim);
            }
        }
    }

    public void forceAssumedShape()
    {
        _forceAssumendShape = true;
    }

    public boolean isForcedAssumedShape()
    {
        return _forceAssumendShape;
    }

    // Type of promotion
    public enum PromotionType {
        SCALAR_TO_ARRAY, ARRAY_TO_ARRAY
    }
}
