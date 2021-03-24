/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.TatsuConstant;

import java.util.ArrayList;
import java.util.List;

/**
 * Enumeration that define the possible target supported. Currently CPU, GPU and
 * MIC.
 *
 * @author clementval
 */
public enum Target {
    ARM(TatsuConstant.TARGET_ARM), CPU(TatsuConstant.TARGET_CPU), GPU(TatsuConstant.TARGET_GPU),
    MIC(TatsuConstant.TARGET_MIC), FPGA(TatsuConstant.TARGET_FPGA), NONE(TatsuConstant.TARGET_NONE);

    private final String _code;

    Target(String code)
    {
        this._code = code;
    }

    public static List<String> availableTargets()
    {
        List<String> codes = new ArrayList<>();
        for (Target t : Target.values())
        {
            codes.add(t._code);
        }
        return codes;
    }

    /**
     * Get enum value from a string.
     *
     * @param value Code value for the enumeration.
     * @return The enumeration value if matches. CPU otherwise.
     */
    public static Target fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value)
        {
        case TatsuConstant.TARGET_ARM:
            return ARM;
        case TatsuConstant.TARGET_CPU:
            return CPU;
        case TatsuConstant.TARGET_GPU:
            return GPU;
        case TatsuConstant.TARGET_MIC:
            return MIC;
        case TatsuConstant.TARGET_FPGA:
            return FPGA;
        default:
            return NONE;
        }
    }

    /**
     * Convert current enum to String value.
     *
     * @return Corresponding String value.
     */
    @Override
    public String toString()
    {
        switch (this)
        {
        case ARM:
            return TatsuConstant.TARGET_ARM;
        case GPU:
            return TatsuConstant.TARGET_GPU;
        case CPU:
            return TatsuConstant.TARGET_CPU;
        case MIC:
            return TatsuConstant.TARGET_MIC;
        case FPGA:
            return TatsuConstant.TARGET_FPGA;
        default:
            return TatsuConstant.TARGET_NONE;
        }
    }
}
