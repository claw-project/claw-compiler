/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;

import java.util.HashMap;
import java.util.Map;

/**
 * The Xscope represents the possible value for the scope attribute in XcodeML
 * intermediate representation.
 *
 * Possible value are: local, global, param
 *
 * @author clementval
 */

public enum Xscope {
    LOCAL(Xname.SCOPE_LOCAL), GLOBAL(Xname.SCOPE_GLOBAL), PARAM(Xname.SCOPE_PARAM);

    private static final Map<String, Xscope> _stringToEnum = new HashMap<>();

    static
    {
        for (Xscope scope : values())
        {
            _stringToEnum.put(scope.toString().toLowerCase(), scope);
        }
    }

    private final String _irValue;

    Xscope(String value)
    {
        _irValue = value;
    }

    /**
     * Convert string value to enum.
     *
     * @param value String value.
     * @return Corresponding enum value. Null if no match found.
     */
    public static Xscope fromString(String value)
    {
        return (value == null || !_stringToEnum.containsKey(value.toLowerCase())) ? null
                : _stringToEnum.get(value.toLowerCase());
    }

    /**
     * Convert current enum to String value.
     *
     * @return Corresponding String value.
     */
    @Override
    public String toString()
    {
        return _irValue;
    }
}
