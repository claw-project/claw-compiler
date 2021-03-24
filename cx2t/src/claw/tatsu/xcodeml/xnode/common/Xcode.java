/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import claw.tatsu.xcodeml.xnode.Xname;

import java.util.HashMap;
import java.util.Map;

/**
 * XcodeML/F element code from the specification 1.0
 *
 * All enum value are written without underscore. All String value are defined
 * in Xname.
 *
 * @author clementval
 */
public enum Xcode {
    ALLOC(Xname.ALLOC), // XcodeML/F 6.30
    ALLOC_OPT(Xname.ALLOC_OPT), // XcodeML/F 6.31
    ARGUMENTS(Xname.ARGUMENTS), // XcodeML/F 7.5.2
    ARRAY_INDEX(Xname.ARRAY_INDEX), // XcodeML/F 8.10
    ASSOCIATE_STATEMENT(Xname.ASSOCIATE_STATEMENT, true), // XcodeML/F 6.42
    BINDING(Xname.BINDING), // XcodeML/F 3.13
    BLOCK_STATEMENT(Xname.BLOCK_STATEMENT, true), // XcodeML/F 6.43
    BODY(Xname.BODY), // XcodeML/F 8.7
    CONDITION(Xname.CONDITION), // XcodeML/F 6.27
    CONTINUE_STATEMENT(Xname.CONTINUE_STATEMENT), // XcodeML/F 6.7
    CO_SHAPE(Xname.CO_SHAPE), // XcodeML/F 3.2
    CRITICAL_STATEMENT(Xname.CRITICAL_STATEMENT, true), // XcodeML/F 6.41
    DECLARATIONS(Xname.DECLARATIONS), // XcodeML/F 5.2
    DEPENDS(Xname.DEPENDS),
    // FortranModule file
    ELSE(Xname.ELSE, true), // XcodeML/F 6.29
    EXPR_STATEMENT(Xname.EXPR_STATEMENT), // XcodeML/F 6.2
    EXTERN_DECL(Xname.EXTERN_DECL), // XcodeML/F 5.6
    FUNCTION_CALL(Xname.FUNCTION_CALL), // XcodeML/F 7.5.1
    F_ALLOCATE_STATEMENT(Xname.F_ALLOCATE_STATEMENT, Xname.ALLOCATE), // F 6.24.1
    F_ARRAY_CONSTRUCTOR(Xname.F_ARRAY_CONSTRUCTOR), // XcodeML/F 7.2.1
    F_ARRAY_REF(Xname.F_ARRAY_REF), // XcodeML/F 7.4.4
    F_ASSIGN_STATEMENT(Xname.F_ASSIGN_STATEMENT), // XcodeML/F 6.1
    F_BACKSPACE_STATEMENT(Xname.F_BACKSPACE_STATEMENT), // XcodeML/F 6.17.3
    F_BASIC_TYPE(Xname.F_BASIC_TYPE), // XcodeML/F 3.13
    F_CASE_LABEL(Xname.F_CASE_LABEL, true), // XcodeML/F 6.14
    F_CHARACTER_REF(Xname.F_CHAR_REF), // XcodeML/F 7.4.5
    F_CLOSE_STATEMENT(Xname.F_CLOSE_STATEMENT), // XcodeML/F 6.17.5
    F_COARRAY_REF(Xname.F_COARRAY_REF), // XcodeML/F 7.4.3
    F_COMMENT_LINE(Xname.F_COMMENT_LINE), //
    F_COMMON_DECL(Xname.F_COMMON_DECL), // XcodeML/F 6.22
    F_CONTAINS_STATEMENT(Xname.F_CONTAINS_STATEMENT), // XcodeML/F 6.26
    F_CYCLE_STATEMENT(Xname.F_CYCLE_STATEMENT), // XcodeML/F 6.8
    F_DATA_DECL(Xname.F_DATA_DECL), // XcodeML/F 6.19
    F_DEALLOCATE_STATEMENT(Xname.F_DEALLOCATE_STATEMENT, Xname.DEALLOCATE), // F 6.24.2
    F_DO_CONCURRENT_STATEMENT(Xname.F_DO_CONCURRENT_STATEMENT, true), // X/F 6.33
    F_DO_LOOP(Xname.F_DO_LOOP), // XcodeML/F 8.15
    F_DO_STATEMENT(Xname.F_DO_STATEMENT, true), // XcodeML/F 6.5
    F_DO_WHILE_STATEMENT(Xname.F_DO_WHILE_STATEMENT, true), // XcodeML/F 6.6
    F_END_FILE_STATEMENT(Xname.F_ENDFILE_STATEMENT), // XcodeML/F 6.17.3
    F_ENTRY_DECL(Xname.F_ENTRY_DECL), // XcodeML/F 6.23
    F_ENUM_DECL(Xname.F_ENUM_DECL), // XcodeML/F 5.14
    F_ENUM_TYPE(Xname.F_ENUM_TYPE), // XcodeML/F 3.14
    F_EQUIVALENCE_DECL(Xname.F_EQUIVALENCE_DECL), // XcodeML/F 6.21
    F_EXIT_STATEMENT(Xname.F_EXIT_STATEMENT), // XcodeML/F 6.9
    F_FLUSH_STATEMENT(Xname.F_FLUSH_STATEMENT), // XcodeML/F 6.17.8
    F_FORMAT_DECL(Xname.F_FORMAT_DECL), // XcodeML/F 6.18
    F_FUNCTION_DEFINITION(Xname.F_FUNCTION_DEFINITION, true), // XcodeML/F 5.3
    F_FUNCTION_TYPE(Xname.F_FUNCTION_TYPE), // XcodeML/F 3.4
    F_IF_STATEMENT(Xname.F_IF_STMT), // XcodeML/F 6.4
    FINAL_PROCEDURE(Xname.FINAL_PROCEDURE), // XcodeML/F 3.12
    F_INT_CONSTANT(Xname.F_INT_CONST), // XcodeML/F 7.1.1
    F_INTERFACE_DECL(Xname.F_INTERFACE_DECL), // XcodeML/F 5.10
    F_INQUIRE_STATEMENT(Xname.F_INQUIRE_STATEMENT), // XcodeML/F 6.17.6
    F_REAL_CONSTANT(Xname.F_REAL_CONST), // XcodeML/F 7.1.2
    F_COMPLEX_CONSTANT(Xname.F_COMPLEX_CONST), // XcodeML/F 7.1.1
    F_COMPLEX_PART_REF(Xname.F_COMPLEX_PART_REF), // XcodeML/F 7.4.7
    F_CHARACTER_CONSTANT(Xname.F_CHAR_CONST), // XcodeML/F 7.1.1
    F_FUNCTION_DECL(Xname.F_FUNCTION_DECL), // XcodeML/F 5.12
    F_LOGICAL_CONSTANT(Xname.F_LOGICAL_CONST), // XcodeML/F 7.1.1
    F_MEMBER_REF(Xname.F_MEMBER_REF), // XcodeML/F 7.4.2
    F_MODULE_DEFINITION(Xname.F_MODULE_DEFINITION), // XcodeML/F 5.7
    F_MODULE_PROCEDURE_DECL(Xname.F_MODULE_PROCEDURE_DECL), // XcodeML/F 5.11
    F_NAMELIST_DECL(Xname.F_NAMELIST_DECL), // XcodeML/F 6.20
    F_NULLIFY_STATEMENT(Xname.F_NULLIFY_STATEMENT), // XcodeML/F 6.24.3
    F_OPEN_STATEMENT(Xname.F_OPEN_STATEMENT), // XcodeML/F 6.17.4
    FOR_ALL_STATEMENT(Xname.FOR_ALL_STATEMENT, true), // XcodeML/F 6.32
    F_POINTER_ASSIGN_STATEMENT(Xname.F_POINTER_ASSIGN_STATEMENT), // XcodeML/F 6.3
    F_PRINT_STATEMENT(Xname.F_PRINT_STATEMENT), // XcodeML/F 6.17.2
    F_PROCEDURE_DECL(Xname.F_PROCEDURE_DECL), // XcodeML/F 5.13
    F_READ_STATEMENT(Xname.F_READ_STATEMENT), // XcodeML/F 6.17.1
    F_RETURN_STATEMENT(Xname.F_RETURN_STATEMENT), // XcodeML/F 6.10
    F_REWIND_STATEMENT(Xname.F_REWIND_STATEMENT), // XcodeML/F 6.17.3
    F_SELECT_CASE_STATEMENT(Xname.F_SELECT_CASE_STATEMENT), // XcodeML/F 6.13
    F_STOP_STATEMENT(Xname.F_STOP_STATEMENT), // XcodeML/F 6.16
    F_STRUCT_CONSTRUCTOR(Xname.F_STRUCT_CONSTRUCTOR), // XcodeML/F 7.3.1
    F_STRUCT_DECL(Xname.F_STRUCT_DECL), // XcodeML/F 5.5
    F_STRUCT_TYPE(Xname.F_STRUCT_TYPE), // XcodeML/F 3.5
    F_USE_DECL(Xname.F_USE_DECL), // XcodeML/F 5.8
    F_USE_ONLY_DECL(Xname.F_USE_ONLY_DECL), // XcodeML/F 5.9
    F_WAIT_STATEMENT(Xname.F_WAIT_STATEMENT), // XcodeML/F 6.17.7
    F_WHERE_STATEMENT(Xname.F_WHERE_STATEMENT), // XcodeML/F 6.15
    F_WRITE_STATEMENT(Xname.F_WRITE_STATEMENT), // XcodeML/F 6.17.1
    GLOBAL_DECLARATIONS(Xname.GLOBAL_DECLARATIONS), // XcodeML/F 5.1
    GLOBAL_SYMBOLS(Xname.GLOBAL_SYMBOLS), // XcodeML/F 4.1
    GOTO_STATEMENT(Xname.GOTO_STATEMENT, Xname.GOTO), // XcodeML/F 6.11
    ID(Xname.ID), // XcodeML/F 8.2
    IDENTIFIERS(Xname.IDENTIFIERS),
    // FortranModule file
    INDEX_RANGE(Xname.INDEX_RANGE), // XcodeML/F 8.11
    KIND(Xname.KIND), // XcodeML/F 8.1
    LEN(Xname.LENGTH), // XcodeML/F 8.6
    LOCK_STATEMENT(Xname.LOCK_STATEMENT), // XcodeML/F 6.39
    LOWER_BOUND(Xname.LOWER_BOUND), // XcodeML/F 8.12
    NAME(Xname.NAME), // XcodeML/F 8.3
    NAMED_VALUE(Xname.NAMED_VALUE), // XcodeML/F 8.16
    NAMED_VALUE_LIST(Xname.NAMED_VALUE_LIST), // XcodeML/F 8.17
    PARAMS(Xname.PARAMS), // XcodeML/F 8.5
    F_PRAGMA_STATEMENT(Xname.F_PRAGMA_STMT), // XcodeML/F 6.25
    RENAME(Xname.RENAME), // XcodeML/F 8.8
    RENAMABLE(Xname.RENAMABLE), // XcodeML/F 8.9
    SELECT_TYPE_STATEMENT(Xname.SELECT_TYPE_STATEMENT), // XcodeML/F 6.34
    STATEMENT_LABEL(Xname.STATEMENT_LABEL), // XcodeML/F 6.12
    STEP(Xname.STEP), // XcodeML/F 8.14
    SYMBOLS(Xname.SYMBOLS), // XcodeML/F 4.2
    SYNC_ALL_STATEMENT(Xname.SYNC_ALL_STATEMENT), // XcodeML/F 6.36
    SYNC_IMAGES_STATEMENT(Xname.SYNC_IMAGES_STATEMENT), // XcodeML/F 6.37
    SYNC_MEMORY_STATEMENT(Xname.SYNC_MEMORY_STATEMENT), // XcodeML/F 6.38
    SYNC_STAT(Xname.SYNC_STAT), // XcodeML/F 6.40
    THEN(Xname.THEN, true), // XcodeML/F 6.28
    TYPE_BOUND_GENERIC_PROCEDURE(Xname.TYPE_BOUND_GENERIC_PROCEDURE), // X/F 3.11
    TYPE_BOUND_PROCEDURES(Xname.TYPE_BOUND_PROCEDURES), // XcodeML/F 3.9
    TYPE_BOUND_PROCEDURE(Xname.TYPE_BOUND_PROCEDURE), // XcodeML/F 3.10
    TYPE_GUARD(Xname.TYPE_GUARD, true), // XcodeML/F 6.53
    TYPE_PARAM(Xname.TYPE_PARAM), // XcodeML/F 3.7
    TYPE_PARAMS(Xname.TYPE_PARAMS), // XcodeML/F 3.6
    TYPE_PARAM_VALUES(Xname.TYPE_PARAM_VALUES), // XcodeML/F 3.8
    TYPE_TABLE(Xname.TYPE_TABLE), // XcodeML/F 3.1
    UNLOCK_STATEMENT(Xname.UNLOCK_STATEMENT), // XcodeML/F 6.39
    UPPER_BOUND(Xname.UPPER_BOUND), // XcodeML/F 8.13
    VALUE(Xname.VALUE), // XcodeML/F 8.4
    VALUE_LIST(Xname.VALUE_LIST), // XcodeML/F 8.18
    VAR(Xname.VAR), // XcodeML/F 7.4.1
    VAR_DECL(Xname.VAR_DECL), // XcodeML/F 5.4
    VAR_LIST(Xname.VAR_LIST), // XcodeML/F 8.19
    VAR_REF(Xname.VAR_REF), // XcodeML/F 7.4.6
    XCODE_PROGRAM(Xname.XCODE_PROGRAM), // XcodeML/F 2

    // Binary expression element
    DIV_EXPR(Xname.DIV_EXPR), // XcodeML/F 7.6
    F_CONCAT_EXPR(Xname.F_CONCAT_EXPR), // XcodeML/F 7.6
    F_POWER_EXPR(Xname.F_POWER_EXPR), // XcodeML/F 7.6
    LOG_AND_EXPR(Xname.LOG_AND_EXPR), // XcodeML/F 7.6
    LOG_EQ_EXPR(Xname.LOG_EQ_EXPR), // XcodeML/F 7.6
    LOG_EQV_EXPR(Xname.LOG_EQV_EXPR), // XcodeML/F 7.6
    LOG_GE_EXPR(Xname.LOG_GE_EXPR), // XcodeML/F 7.6
    LOG_GT_EXPR(Xname.LOG_GT_EXPR), // XcodeML/F 7.6
    LOG_LE_EXPR(Xname.LOG_LE_EXPR), // XcodeML/F 7.6
    LOG_LT_EXPR(Xname.LOG_LT_EXPR), // XcodeML/F 7.6
    LOG_NEQ_EXPR(Xname.LOG_NEQ_EXPR), // XcodeML/F 7.6
    LOG_NEWV_EXPR(Xname.LOG_NEWV_EXPR), // XcodeML/F 7.6
    LOG_OR_EXPR(Xname.LOG_OR_EXPR), // XcodeML/F 7.6
    MINUS_EXPR(Xname.MINUS_EXPR), // XcodeML/F 7.6
    MUL_EXPR(Xname.MUL_EXPR), // XcodeML/F 7.6
    PLUS_EXPR(Xname.PLUS_EXPR), // XcodeML/F 7.6
    USER_BINARY_EXPR(Xname.USER_BINARY_EXPR), // XcodeML/F 7.6

    // Unary expression element
    LOG_NOT_EXPR(Xname.LOG_NOT_EXPR), // XcodeML/F 7.7
    UNARY_MINUS_EXPR(Xname.UNARY_MINUS_EXPR), // XcodeML/F 7.7
    USER_UNARY_EXPR(Xname.USER_UNARY_EXPR), // XcodeML/F 7.7

    // Special opcode for unknown node
    NONE(Xname.NONE);

    private static final Map<String, Xcode> _stringToEnum = new HashMap<>();

    static
    {
        for (Xcode code : values())
        {
            _stringToEnum.put(code.toString().toLowerCase(), code);
        }
    }

    private final String _irValue;
    private final String _fortranValue;
    private final boolean _hasBody;

    Xcode(String s)
    {
        this(s, false);
    }

    Xcode(String s, String fortran)
    {
        this(s, fortran, false);
    }

    Xcode(String s, boolean hasBody)
    {
        _irValue = s;
        _fortranValue = "";
        _hasBody = hasBody;
    }

    Xcode(String s, String fortran, boolean hasBody)
    {
        _irValue = s;
        _fortranValue = fortran;
        _hasBody = hasBody;
    }

    public static Xcode fromString(String value)
    {
        return (value == null || !_stringToEnum.containsKey(value.toLowerCase())) ? NONE
                : _stringToEnum.get(value.toLowerCase());
    }

    @Override
    public String toString()
    {
        return _irValue;
    }

    /**
     * Get the XcodeML IR original code.
     *
     * @return XcodeML code.
     */
    public String code()
    {
        return _irValue;
    }

    /**
     * Get the Fortran keyword value of this Xcode.
     *
     * @return Fortran value.
     */
    public String fortran()
    {
        return _fortranValue;
    }

    public boolean hasBody()
    {
        return _hasBody;
    }
}
