/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.abstraction;

import java.util.List;

/**
 * Store the information from a reshape clause in a CLAW directives.
 *
 * @author clementval
 */
public class ReshapeInfo
{

    private final String _array_name;
    private final int _target_dimension;
    private final List<Integer> _kept_dimensions;

    /**
     * Constructs a new ReshapeInfo object with all its needed information.
     *
     * @param arrayName      Identifier of the array.
     * @param dimension      Number of dimension after reshape transformation.
     * @param keptDimensions Optional, if target dimension is bigger than 0 then
     *                       this list informs which dimension is preserved.
     */
    public ReshapeInfo(String arrayName, int dimension, List<Integer> keptDimensions)
    {
        _array_name = arrayName;
        _target_dimension = dimension;
        _kept_dimensions = keptDimensions;
    }

    /**
     * Get the extracted array name.
     *
     * @return Array name as string value.
     */
    public String getArrayName()
    {
        return _array_name;
    }

    /**
     * Get the target dimension extracted from the reshape clause.
     *
     * @return Target dimension value.
     */
    public int getTargetDimension()
    {
        return _target_dimension;
    }

    /**
     * Get the extracted kept dimension as a list of strings.
     *
     * @return List of kept dimensions if any. List is empty if this information is
     *         not present.
     */
    public List<Integer> getKeptDimensions()
    {
        return _kept_dimensions;
    }
}
