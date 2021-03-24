/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.language;

/**
 * ClawMappingVar represents the mapping variable used in a loop-extract
 * transformation.
 *
 * In the following mapping option:
 *
 * map(v1,v2,v3:i/j)
 *
 * The mapping variable i is the "arg" mapping as used as the mapping variable
 * in the function call arguments. The mapping variable j is the "fct" mapping
 * as used as the mapping variable in the function body.
 *
 * @author clementval
 */
public class ClawMappingVar
{

    private final String _argPart;
    private final String _fctPart;

    /**
     * Constructs a new ClawMappingVar.
     *
     * @param argMapping Function call mapping variable.
     * @param fctMapping Function body mapping variable.
     */
    public ClawMappingVar(String argMapping, String fctMapping)
    {
        _argPart = argMapping;
        _fctPart = fctMapping;
    }

    /**
     * Check whether the mapping variable has two different variables for the
     * function call arguments mapping and the function body mapping.
     *
     * @return True if the two mapping are different. False otherwise.
     */
    public boolean hasDifferentMapping()
    {
        return !_argPart.equals(_fctPart);
    }

    /**
     * Get the function call argument mapping part
     *
     * @return Mapping variable
     */
    public String getArgMapping()
    {
        return _argPart;
    }

    /**
     * Get the function body mapping part
     *
     * @return Mapping variable
     */
    public String getFctMapping()
    {
        return _fctPart;
    }

    /**
     * Return a string representation of the object.
     *
     * @return String representation of the ClawMapping object.
     */
    @Override
    public String toString()
    {
        if (hasDifferentMapping())
        {
            return getArgMapping() + "/" + getFctMapping();
        }
        return getArgMapping();
    }
}
