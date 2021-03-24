/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.TatsuConstant;

/**
 * Define the possible execution mode for OpenACC.
 *
 * @author clementval
 */
public enum OpenAccExecutionMode {
    NONE, VECTOR, GANG, GANG_VECTOR;

    /**
     * Get enum value from a string.
     *
     * @param value Code value for the enumeration.
     * @return The enumeration value if matches. NONE otherwise.
     */
    public static OpenAccExecutionMode fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value)
        {
        case TatsuConstant.OPENACC_EXEC_MODE_VECTOR:
            return VECTOR;
        case TatsuConstant.OPENACC_EXEC_MODE_GANG:
            return GANG;
        case TatsuConstant.OPENACC_EXEC_MODE_GANG_VECTOR:
            return GANG_VECTOR;
        case TatsuConstant.OPENACC_EXEC_MODE_NONE:
        default:
            return NONE;
        }
    }

    /**
     * Get string version of the execution mode.
     *
     * @return OpenACC clause according to the execution mode.
     */
    public String getFormattedExecutionMode()
    {
        switch (this)
        {
        case NONE:
            return "";
        case GANG_VECTOR:
            return "gang vector";
        case GANG:
            return "gang";
        case VECTOR:
            return "vector";
        }
        return "";
    }
}
