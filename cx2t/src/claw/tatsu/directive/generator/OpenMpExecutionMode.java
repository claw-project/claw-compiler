/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.generator;

import claw.tatsu.TatsuConstant;

/**
 * Define the possible execution mode for OpenMP.
 *
 * @author peclatj
 */
public enum OpenMpExecutionMode {
    NONE, TEAMS_DISTRIBUTE, TEAMS_DISTRIBUTE_SIMD, TEAMS_DISTRIBUTE_PARALLEL_DO, TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;

    /**
     * Get enum value from a string.
     *
     * @param value Code value for the enumeration.
     * @return The enumeration value if matches. NONE otherwise.
     */
    public static OpenMpExecutionMode fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value)
        {
        case TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE:
            return TEAMS_DISTRIBUTE;
        case TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_SIMD:
            return TEAMS_DISTRIBUTE_SIMD;
        case TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_PARALLEL_DO:
            return TEAMS_DISTRIBUTE_PARALLEL_DO;
        case TatsuConstant.OPENMP_EXEC_MODE_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
            return TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD;
        case TatsuConstant.OPENMP_EXEC_MODE_NONE:
        default:
            return NONE;
        }
    }

    /**
     * Get string version of the execution mode.
     *
     * @return OpenMP clause according to the execution mode.
     */
    public String getFormattedExecutionMode()
    {
        switch (this)
        {
        case TEAMS_DISTRIBUTE:
            return "teams distribute";
        case TEAMS_DISTRIBUTE_SIMD:
            return "teams distribute simd";
        case TEAMS_DISTRIBUTE_PARALLEL_DO:
            return "teams distribute parallel do";
        case TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
            return "teams distribute parallel do simd";
        case NONE:
        default:
            return "";
        }
    }
}
