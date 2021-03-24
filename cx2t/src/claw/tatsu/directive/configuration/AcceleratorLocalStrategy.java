/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.directive.configuration;

/**
 * Define the enum values for the local strategy for OpenACC
 *
 * @author clementval
 */
public enum AcceleratorLocalStrategy {
    PRIVATE, PROMOTE;

    static final String LOCAL_STRATEGY_PRIVATE = "private";
    static final String LOCAL_STRATEGY_PROMOTE = "promote";

    /**
     * Get enum value from configuration string.
     *
     * @param value String value from configuration.
     * @return Enum value corresponding to the string. Default is PRIVATE.
     */
    public static AcceleratorLocalStrategy fromString(String value)
    {
        if (value == null)
        {
            return PRIVATE;
        }
        switch (value)
        {
        case LOCAL_STRATEGY_PROMOTE:
            return PROMOTE;
        case LOCAL_STRATEGY_PRIVATE:
        default:
            return PRIVATE;
        }
    }
}
