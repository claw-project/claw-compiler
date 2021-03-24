/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.common;

import claw.tatsu.TatsuConstant;
import claw.tatsu.directive.generator.OpenAcc;
import claw.tatsu.directive.generator.OpenMp;

import java.util.ArrayList;
import java.util.List;

/**
 * Enumeration that define the possible directive directive supported. Currently
 * OpenACC, OpenMP and NONE are available.
 *
 * @author clementval
 */
public enum CompilerDirective {
    NONE(TatsuConstant.DIRECTIVE_NONE, ""), OPENACC(OpenAcc.OPENACC_NAME, OpenAcc.OPENACC_PREFIX),
    OPENMP(OpenMp.OPENMP_NAME, OpenMp.OPENMP_PREFIX), CLAW(TatsuConstant.DIRECTIVE_CLAW, TatsuConstant.CLAW_PREFIX);

    private final String _code;
    private final String _prefix;

    CompilerDirective(String code, String prefix)
    {
        _code = code;
        _prefix = prefix;
    }

    public static List<String> availableDirectiveLanguage()
    {
        List<String> codes = new ArrayList<>();
        for (CompilerDirective t : CompilerDirective.values())
        {
            codes.add(t.getCode());
        }
        return codes;
    }

    /**
     * Get enum value from a string.
     *
     * @param value Code value for the enumeration.
     * @return The enumeration value if matches. NONE otherwise.
     */
    public static CompilerDirective fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value.toLowerCase())
        {
        case OpenAcc.OPENACC_NAME:
        case OpenAcc.OPENACC_PREFIX:
            return OPENACC;
        case OpenMp.OPENMP_NAME:
        case OpenMp.OPENMP_PREFIX:
            return OPENMP;
        case TatsuConstant.DIRECTIVE_NONE:
        default:
            return NONE;
        }
    }

    /**
     * Get the compiler directive code name.
     *
     * @return Compiler directive code name.
     */
    private String getCode()
    {
        return _code;
    }

    /**
     * Get the compiler directive prefix.
     *
     * @return Compiler directive prefix.
     */
    public String getPrefix()
    {
        return _prefix;
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
        case OPENACC:
            return OpenAcc.OPENACC_NAME;
        case OPENMP:
            return OpenMp.OPENMP_NAME;
        default:
            return TatsuConstant.DIRECTIVE_NONE;
        }
    }
}
