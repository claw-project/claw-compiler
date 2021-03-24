/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

import java.util.List;
import java.util.stream.Collectors;

/**
 * A ClawMapping object holds the loop-extract mapping clause representation
 * var_list:mapping_list
 *
 * @author clementval
 */

public class ClawMapping
{

    private List<ClawMappingVar> _mappedVariables = null;
    private List<ClawMappingVar> _mappingVariables = null;

    /**
     * Get a list of all mapping variables.
     *
     * @return List of mapping variable as ClawMappingVar.
     */
    public List<ClawMappingVar> getMappingVariables()
    {
        return _mappingVariables;
    }

    /**
     * Set the mapping variable list.
     *
     * @param mappingVars List of ClawMappingVar that represents the mapping
     *                    variables.
     */
    public void setMappingVariables(List<ClawMappingVar> mappingVars)
    {
        _mappingVariables = mappingVars;
    }

    /**
     * Get a list of all mapped variables.
     *
     * @return List of mapped variable as String.
     */
    public List<ClawMappingVar> getMappedVariables()
    {
        return _mappedVariables;
    }

    /**
     * Set the mapped variable list.
     *
     * @param mappedVars List of ClawMappingVar that represents the mapped
     *                   variables.
     */
    public void setMappedVariables(List<ClawMappingVar> mappedVars)
    {
        _mappedVariables = mappedVars;
    }

    /**
     * get the number of dimension mapped in this ClawMapping object
     *
     * @return the number of dimension to be mapped
     */
    public int getMappedDimensions()
    {
        return _mappingVariables.size();
    }

    /**
     * Return a string representation of the object.
     *
     * @return String representation of the ClawMapping object.
     */
    @Override
    public String toString()
    {
        return getMappedVariables().stream().map(ClawMappingVar::toString).collect(Collectors.joining(",")) + ":"
                + getMappingVariables().stream().map(ClawMappingVar::toString).collect(Collectors.joining(","));
    }
}
