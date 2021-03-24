/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.configuration;

/**
 * Define the enum values for the data strategy for OpenACC and OpenMP
 *
 * @author clementval
 */
public enum AcceleratorDataStrategy {
    NONE, PRESENT, KERNEL;

    static final String DATA_STRATEGY_NONE = "none";
    static final String DATA_STRATEGY_PRESENT = "present";
    static final String DATA_STRATEGY_KERNEL = "kernel";

    /**
     * Get enum value from configuration string.
     *
     * @param value String value from configuration.
     * @return Enum value corresponding to the string. Default is NONE.
     */
    public static AcceleratorDataStrategy fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value.toLowerCase())
        {
        case DATA_STRATEGY_KERNEL:
            return KERNEL;
        case DATA_STRATEGY_PRESENT:
            return PRESENT;
        case DATA_STRATEGY_NONE:
        default:
            return NONE;
        }
    }
}
