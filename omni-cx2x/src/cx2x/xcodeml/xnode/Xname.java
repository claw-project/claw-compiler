/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * Xname class contains all element and attributes values that can be
 * found in the XcodeML/F intermediate representation language.
 *
 * @author clementval
 */

public class Xname {

  // helpers
  public static final String TRUE = "true";
  public static final String FALSE = "false";
  public static final String SUPPORTED_VERSION = "1.0";
  public static final String SUPPORTED_LANGUAGE = "Fortran";
  public static final String DEFAULT_STEP_VALUE = "1";

  // Base type (XcodeML/F 0.91J 9.1)
  public static final String TYPE_F_INT = "Fint";
  public static final String TYPE_F_REAL = "Freal";
  public static final String TYPE_F_COMPLEX = "Fcomplex";
  public static final String TYPE_F_LOGICAL = "Flogical";
  public static final String TYPE_F_CHAR = "Fcharacter";
  public static final String TYPE_F_VOID = "Fvoid";

  // Storage class (part of Id - XcodeML/F 0.91J 8.2)
  public static final String SCLASS_AUTO = "auto";
  public static final String SCLASS_EXTERN = "extern";
  public static final String SCLASS_EXTERN_DEF = "extern_def";
  public static final String SCLASS_F_LOCAL = "flocal";
  public static final String SCLASS_F_FUNC = "ffunc";
  public static final String SCLASS_F_PARAM = "fparam";
  public static final String SCLASS_LABEL = "label";
  public static final String SCLASS_PARAM = "param";

  // Scope (part of Var - XcodeML/F 0.91J 7.4.1)
  public static final String SCOPE_LOCAL = "local";
  public static final String SCOPE_GLOBAL = "global";
  public static final String SCOPE_PARAM = "param";

  // Intent (part of FbasicType - XcodeML/F 0.91J 3.3)
  public static final String INTENT_IN = "in";
  public static final String INTENT_OUT = "out";
  public static final String INTENT_INOUT = "inout";

  // Element attributes
  public static final String ATTR_COMPILER_INFO = "compiler-info";
  public static final String ATTR_CONSTRUCT_NAME = "construct_name";
  public static final String ATTR_DATAREF = "data_ref";
  public static final String ATTR_FILE = "file";
  public static final String ATTR_INTENT = "intent";
  public static final String ATTR_IS_ASSUMED_SHAPE = "is_assumed_shape";
  public static final String ATTR_IS_ALLOCATABLE = "is_allocatable";
  public static final String ATTR_IS_EXTERNAL = "is_external";
  public static final String ATTR_IS_INTERNAL = "is_internal";
  public static final String ATTR_IS_INTRINSIC = "is_intrinsic";
  public static final String ATTR_IS_OPERATOR = "is_operator";
  public static final String ATTR_IS_OPTIONAL = "is_optional";
  public static final String ATTR_IS_PARAMETER = "is_parameter";
  public static final String ATTR_IS_POINTER = "is_pointer";
  public static final String ATTR_IS_PRIVATE = "is_private";
  public static final String ATTR_IS_PROGRAM = "is_program";
  public static final String ATTR_IS_PUBLIC = "is_public";
  public static final String ATTR_IS_PURE = "is_pure";
  public static final String ATTR_IS_RECURSIVE = "is_recursive";
  public static final String ATTR_IS_SAVE = "is_save";
  public static final String ATTR_IS_TARGET = "is_target";
  public static final String ATTR_KIND = "kind";
  public static final String ATTR_LANGUAGE = "language";
  public static final String ATTR_LINENO = "lineno";
  public static final String ATTR_LOCAL_NAME = "local_name";
  public static final String ATTR_NAME = "name";
  public static final String ATTR_REPEAT_COUNT = "repeat_count";
  public static final String ATTR_REF = "ref";
  public static final String ATTR_RESULT_NAME = "result_name";
  public static final String ATTR_RETURN_TYPE = "return_type";
  public static final String ATTR_SCLASS = "sclass";
  public static final String ATTR_SCOPE = "scope";
  public static final String ATTR_SOURCE = "source";
  public static final String ATTR_TIME = "time";
  public static final String ATTR_TYPE = "type";
  public static final String ATTR_USE_NAME = "use_name";
  public static final String ATTR_VERSION = "version";

  // Element names
  public static final String ALLOC = "alloc";
  public static final String ARGUMENTS = "arguments";
  public static final String ARRAY_INDEX = "arrayIndex";
  public static final String BASIC_TYPE = "FbasicType";
  public static final String BODY = "body";
  public static final String CONDITION = "condition";
  public static final String CO_SHAPE = "coShape";
  public static final String CONTINUE_STATEMENT = "continueStatement";
  public static final String DECLARATIONS = "declarations";
  public static final String ELSE = "else";
  public static final String EXPR_STMT = "exprStatement";
  public static final String EXTERN_DECL = "externDecl";
  public static final String FINAL_PROCEDURE = "finalProcedure";
  public static final String FUNCTION_CALL = "functionCall";
  public static final String F_ALLOCATE_STATEMENT = "FallocateStatement";
  public static final String F_ARRAY_CONSTRUCTOR = "FarrayConstructor";
  public static final String F_ARRAY_REF = "FarrayRef";
  public static final String F_ASSIGN_STMT = "FassignStatement";
  public static final String F_BACKSPACE_STATEMENT = "FbackspaceStatement";
  public static final String F_CASE_LABEL = "FcaseLabel";
  public static final String F_CHAR_REF = "FcharacterRef";
  public static final String F_COARRAY_REF = "FcoArrayRef";
  public static final String F_COMMON_DECL = "FcommonDecl";
  public static final String F_CONTAINS_STATEMENT = "FcontainsStatement";
  public static final String F_CYCLE_STATEMENT = "FcycleStatement";
  public static final String F_DATA_DECL = "FdataDecl";
  public static final String F_DEALLOCATE_STATEMENT = "FdeallocateStatement";
  public static final String F_DO_LOOP = "FdoLoop";
  public static final String F_DO_STATEMENT = "FdoStatement";
  public static final String F_DO_WHILE_STATEMENT = "FdoWhileStatement";
  public static final String F_ENDFILE_STATEMENT = "FendFileStatement";
  public static final String F_ENTRY_DECL = "FentryDecl";
  public static final String F_EQUIVALENCE_DECL = "FequivalenceDecl";
  public static final String F_EXIT_STATEMENT = "FexitStatement";
  public static final String F_FORMAT_DECL = "FformatDecl";
  public static final String F_FUNCTION_DEFINITION = "FfunctionDefinition";
  public static final String F_FUNCTION_TYPE = "FfunctionType";
  public static final String F_IF_STMT = "FifStatement";
  public static final String F_INT_CONST = "FintConstant";
  public static final String F_INTERFACE_DECL = "FinterfaceDecl";
  public static final String F_INQUIRE_STATEMENT = "FinquireStatement";
  public static final String F_REAL_CONST = "FrealConstant";
  public static final String F_CLOSE_STATEMENT = "FcloseStatement";
  public static final String F_COMPLEX_CONST = "FcomplexConstant";
  public static final String F_CHAR_CONST = "FcharacterConstant";
  public static final String F_FUNCTION_DECL = "FfunctionDecl";
  public static final String F_LOGICAL_CONST = "FlogicalConstant";
  public static final String F_MEMBER_REF = "FmemberRef";
  public static final String F_MODULE_DEFINITION = "FmoduleDefinition";
  public static final String F_MODULE_PROCEDURE_DECL = "FmoduleProcedureDecl";
  public static final String F_NAMELIST_DECL = "FnamelistDecl";
  public static final String F_NULLIFY_STATEMENT = "FnullifyStatement";
  public static final String F_OPEN_STATEMENT = "FopenStatement";
  public static final String F_POINTER_ASSIGN_STATEMENT = "FpointerAssignStatement";
  public static final String F_PRINT_STATEMENT = "FprintStatement";
  public static final String F_READ_STATEMENT = "FreadStatement";
  public static final String F_RETURN_STATEMENT = "FreturnStatement";
  public static final String F_REWIND_STATEMENT = "FrewindStatement";
  public static final String F_SELECT_CASE_STATEMENT = "FselectCaseStatement";
  public static final String F_STOP_STATEMENT = "FstopStatement";
  public static final String F_STRUCT_CONSTRUCTOR = "FstructConstructor";
  public static final String F_STRUCT_DECL = "FstructDecl";
  public static final String F_STRUCT_TYPE = "FstructType";
  public static final String F_USE_DECL = "FuseDecl";
  public static final String F_USE_ONLY_DECL = "FuseOnlyDecl";
  public static final String F_WHERE_STATEMENT = "FwhereStatement";
  public static final String F_WRITE_STATEMENT = "FwriteStatement";
  public static final String GLOBAL_DECLARATIONS = "globalDeclarations";
  public static final String GLOBAL_SYMBOLS = "globalSymbols";
  public static final String GOTO_STATEMENT = "gotoStatement";
  public static final String ID = "id";
  public static final String INDEX_RANGE = "indexRange";
  public static final String KIND = "kind";
  public static final String LENGTH = "len";
  public static final String LOWER_BOUND = "lowerBound";
  public static final String NAME = "name";
  public static final String NAMED_VALUE = "namedValue";
  public static final String NAMED_VALUE_LIST = "namedValueList";
  public static final String PARAMS = "params";
  public static final String PRAGMA_STMT = "FpragmaStatement";
  public static final String RENAME = "rename";
  public static final String RENAMABLE = "renamable";
  public static final String SELECT_TYPE_STATEMENT = "selectTypeStatement";
  public static final String STATEMENT_LABEL = "statementLabel";
  public static final String STEP = "step";
  public static final String SYMBOLS = "symbols";
  public static final String THEN = "then";
  public static final String TYPE_TABLE = "typeTable";
  public static final String TYPE_GUARD = "typeGuard";
  public static final String TYPE_BOUND_GENERIC_PROCEDURE = "typeBoundGenericProcedure";
  public static final String TYPE_BOUND_PROCEDURE = "typeBoundProcedure";
  public static final String TYPE_BOUND_PROCEDURES = "typeBoundProcedures";
  public static final String UPPER_BOUND = "upperBound";
  public static final String VALUE = "value";
  public static final String VALUE_LIST = "valueList";
  public static final String VAR = "Var";
  public static final String VAR_DECL = "varDecl";
  public static final String VAR_LIST = "varList";
  public static final String VAR_REF = "varRef";

  public static final String X_CODE_PROGRAM = "XcodeProgram";

  // Xmod files
  public static final String DEPENDS = "depends";
  public static final String IDENTIFIERS = "identifiers";

  // Binary expression element
  public static final String DIV_EXPR = "divExpr";
  public static final String F_CONCAT_EXPR = "FconcatExpr";
  public static final String F_POWER_EXPR = "FpowerExpr";
  public static final String LOG_AND_EXPR = "logAndExpr";
  public static final String LOG_EQ_EXPR = "logEQExpr";
  public static final String LOG_EQV_EXPR = "logEQVExpr";
  public static final String LOG_GE_EXPR = "logGEExpr";
  public static final String LOG_GT_EXPR = "logGTExpr";
  public static final String LOG_LE_EXPR = "logLEExpr";
  public static final String LOG_LT_EXPR = "logLTExpr";
  public static final String LOG_NEQ_EXPR = "logNEQExpr";
  public static final String LOG_NEWV_EXPR = "logNEWVExpr";
  public static final String LOG_OR_EXPR = "logOrExpr";
  public static final String MINUS_EXPR = "minusExpr";
  public static final String MUL_EXPR = "mulExpr";
  public static final String PLUS_EXPR = "plusExpr";
  public static final String USER_BINARY_EXPR = "userBinaryExpr";

  // Unary expression element
  public static final String LOG_NOT_EXPR = "logNotExpr";
  public static final String UNARY_MINUS_EXPR = "unaryMinusExpr";
  public static final String USER_UNARY_EXPR = "userUnaryExpr";

  // Intrinsic function
  public static final String INTRINSIC_SIZE = "size";
}
