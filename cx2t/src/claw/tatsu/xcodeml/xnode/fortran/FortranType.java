/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import claw.tatsu.xcodeml.xnode.Xname;

import java.util.Random;

/**
 * Enum representing the type coming from OMNI Compiler.
 *
 * @author clementval
 */
public enum FortranType {

    // Type prefix from OMNI Compiler. Taken for the F-output-xcodeml.c file.
    ARRAY('A', ""), CHARACTER('C', Xname.TYPE_F_CHAR), COMPLEX('P', Xname.TYPE_F_COMPLEX), FUNCTION('F', ""),
    INTEGER('I', Xname.TYPE_F_INT), REAL('R', Xname.TYPE_F_REAL), LOGICAL('L', Xname.TYPE_F_LOGICAL), STRUCT('S', ""),
    VOID('V', Xname.TYPE_F_VOID), NONE('N', "");

    // Java 1.8 should use SecureRandom
    private final Random rand = new Random();

    private static final int HASH_LENGTH = 12;
    private final char _prefix;
    private final String _irValue;

    FortranType(char prefix, String type)
    {
        _prefix = prefix;
        _irValue = type;
    }

    /**
     * Check whether the given type is a built-in type or is a type defined in the
     * type table.
     *
     * @param type Type to check.
     * @return True if the type is built-in. False otherwise.
     */
    public static boolean isBuiltInType(String type)
    {
        if (type == null)
        {
            return false;
        }
        switch (type)
        {
        case Xname.TYPE_F_CHAR:
        case Xname.TYPE_F_COMPLEX:
        case Xname.TYPE_F_INT:
        case Xname.TYPE_F_LOGICAL:
        case Xname.TYPE_F_REAL:
        case Xname.TYPE_F_VOID:
            return true;
        default:
            return false;
        }
    }

    /**
     * Get type from XcodeML/F IR value.
     *
     * @param value Type value from XcodeML/F IR.
     * @return Corresponding enum value. NONE if value corresponds to nothing.
     */
    public static FortranType fromString(String value)
    {
        if (value == null)
        {
            return NONE;
        }
        switch (value)
        {
        case Xname.TYPE_F_INT:
            return INTEGER;
        case Xname.TYPE_F_REAL:
            return REAL;
        case Xname.TYPE_F_COMPLEX:
            return COMPLEX;
        case Xname.TYPE_F_LOGICAL:
            return LOGICAL;
        case Xname.TYPE_F_CHAR:
            return CHARACTER;
        case Xname.TYPE_F_VOID:
            return VOID;
        default:
            return NONE;
        }
    }

    /**
     * Get a new unique hash with the current FortranType prefix.
     *
     * @return The new unique hash.
     */
    public String generateHash()
    {
        StringBuilder sb = new StringBuilder();
        while (sb.length() < HASH_LENGTH)
        {
            sb.append(Integer.toHexString(this.rand.nextInt()));
        }
        return _prefix + sb.toString().substring(0, HASH_LENGTH);
    }

    /**
     * Check whether the giving hash is from the current type.
     *
     * @param hash Hash to be checked.
     * @return True if the hash is of current type. False otherwise.
     */
    public boolean isOfType(String hash)
    {
        return hash != null && !hash.isEmpty() && (hash.charAt(0) == _prefix || hash.equals(_irValue));
    }

    @Override
    public String toString()
    {
        return _irValue.isEmpty() ? null : _irValue;
    }
}
