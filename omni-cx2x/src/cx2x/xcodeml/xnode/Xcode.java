/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

/**
 * XcodeML element code.
 *
 * @author clementval
 */
public enum Xcode {
  ALLOC(Xname.ALLOC),                                        // XcodeML 6.30
  ARGUMENTS(Xname.ARGUMENTS),                                // XcodeML 7.5.2
  ARRAYINDEX(Xname.ARRAY_INDEX),                             // XcodeML 8.10
  FBASICTYPE(Xname.BASIC_TYPE),                              // XcodeML 3.3
  BODY(Xname.BODY),                                          // XcodeML 8.7
  CONDITION(Xname.CONDITION),                                // XcodeML 6.27
  CONTINUESTATEMENT(Xname.CONTINUE_STATEMENT),               // XcodeML 6.7
  COSHAPE(Xname.CO_SHAPE),                                   // XcodeML 3.2
  DECLARATIONS(Xname.DECLARATIONS),                          // XcodeML 5.2
  ELSE(Xname.ELSE),                                          // XcodeML 6.29
  EXPRSTATEMENT(Xname.EXPR_STMT),                            // XcodeML 6.2
  EXTERNDECL(Xname.EXTERN_DECL),                             // XcodeML 5.6
  FUNCTIONCALL(Xname.FCT_CALL),                              // XcodeML 7.5.1
  FFUNCTIONDEFINITION(Xname.FCT_DEFINITION),                 // XcodeML 5.3
  FFUNCTIONTYPE(Xname.FCT_TYPE),                             // XcodeML 3.4
  FARRAYCONSTRUCTOR(Xname.F_ARRAY_CONSTRUCTOR),              // XcodeML 7.2.1
  FARRAYREF(Xname.F_ARRAY_REF),                              // XcodeML 7.4.4
  FASSIGNSTATEMENT(Xname.F_ASSIGN_STMT),                     // XcodeML 6.1
  FCASELABEL(Xname.F_CASE_LABEL),                            // XcodeML 6.14
  FCHARACTERREF(Xname.F_CHAR_REF),                           // XcodeML 7.4.5
  FCOARRAYREF(Xname.F_COARRAY_REF),                          // XcodeML 7.4.3
  FCONTAINSSTATEMENT(Xname.F_CONTAINS_STATEMENT),            // XcodeML 6.26
  FCYCLESTATEMENT(Xname.F_CYCLE_STATEMENT),                  // XcodeML 6.8
  FDOLOOP(Xname.F_DO_LOOP),                                  // XcodeML 8.15
  FDOSTATEMENT(Xname.F_DO_STATEMENT),                        // XcodeML 6.5
  FDOWHILESTATEMENT(Xname.F_DO_WHILE_STATEMENT),             // XcodeML 6.6
  FEXITSTATEMENT(Xname.F_EXIT_STATEMENT),                    // XcodeML 6.9
  FIFSTATEMENT(Xname.F_IF_STMT),                             // XcodeML 6.4
  FINTCONSTANT(Xname.F_INT_CONST),                           // XcodeML 7.1.1
  FINTERFACEDECL(Xname.F_INTERFACE_DECL),                    // XcodeML 5.10
  FREALCONSTANT(Xname.F_REAL_CONST),                         // XcodeML 7.1.2
  FCOMPLEXCONSTANT(Xname.F_COMPLEX_CONST),                   // XcodeML 7.1.1
  FCHARACTERCONSTANT(Xname.F_CHAR_CONST),                    // XcodeML 7.1.1
  FFUNCTIONDECL(Xname.F_FUNCTION_DECL),                      // XcodeML 5.12
  FLOGICALCONSTANT(Xname.F_LOGICAL_CONST),                   // XcodeML 7.1.1
  FMEMBERREF(Xname.F_MEMBER_REF),                            // XcodeML 7.4.2
  FMODULEDEFINITION(Xname.F_MODULE_DEFINITION),              // XcodeML 5.7
  FMODULEPROCEDUREDECL(Xname.F_MODULE_PROCEDURE_DECL),       // XcodeML 5.11
  FPOINTERASSIGNSTATEMENT(Xname.F_POINTER_ASSIGN_STATEMENT), // XcodeML 6.3
  FRETURNSTATEMENT(Xname.F_RETURN_STATEMENT),                // XcodeML 6.10
  FSELECTCASESTATEMENT(Xname.F_SELECT_CASE_STATEMENT),       // XcodeML 6.13
  FSTOPSTATEMENT(Xname.F_STOP_STATEMENT),                    // XcoceML 6.16
  FSTRUCTCONSTRUCTOR(Xname.F_STRUCT_CONSTRUCTOR),            // XcodeML 7.3.1
  FSTRUCTDECL(Xname.F_STRUCT_DECL),                          // XcodeML 5.5
  FSTRUCTTYPE(Xname.F_STRUCT_TYPE),                          // XcodeML 3.5
  FUSEDECL(Xname.F_USE_DECL),                                // XcodeML 5.8
  FUSEONLYDECL(Xname.F_USE_ONLY_DECL),                       // XcodeML 5.9
  FWHERESTATEMENT(Xname.F_WHERE_STATEMENT),                  // XcodeML 6.15
  GLOBALDECLARATIONS(Xname.GLOBAL_DECLARATIONS),             // XcodeML 5.1
  GLOBALSYMBOLS(Xname.GLOBAL_SYMBOLS),                       // XcodeML 4.1
  GOTOSTATEMENT(Xname.GOTO_STATEMENT),                       // XcodeML 6.11
  ID(Xname.ID),                                              // XcodeML 8.2
  INDEXRANGE(Xname.INDEX_RANGE),                             // XcodeML 8.11
  KIND(Xname.KIND),                                          // XcodeML 8.1
  LEN(Xname.LENGTH),                                         // XcodeML 8.6
  LOWERBOUND(Xname.LOWER_BOUND),                             // XcodeML 8.12
  NAME(Xname.NAME),                                          // XcodeML 8.3
  NAMEDVALUE(Xname.NAMED_VALUE),                             // XcodeML 8.16
  NAMEDVALUELIST(Xname.NAMED_VALUE_LIST),                    // XcodeML 8.17
  PARAMS(Xname.PARAMS),                                      // XcodeML 8.5
  FPRAGMASTATEMENT(Xname.PRAGMA_STMT),                       // XcodeML 6.25
  RENAME(Xname.RENAME),                                      // XcodeML 8.8
  RENAMABLE(Xname.RENAMABLE),                                // XcodeML 8.9
  STATEMENTLABEL(Xname.STATEMENT_LABEL),                     // XcodeML 6.12
  STEP(Xname.STEP),                                          // XcodeML 8.14
  SYMBOLS(Xname.SYMBOLS),                                    // XcodeML 4.2
  THEN(Xname.THEN),                                          // XcodeML 6.28
  TYPETABLE(Xname.TYPE_TABLE),                               // XcodeML 3.1
  UPPERBOUND(Xname.UPPER_BOUND),                             // XcodeML 8.13
  VALUE(Xname.VALUE),                                        // XcodeML 8.4
  VALUELIST(Xname.VALUE_LIST),                               // XcodeML 8.18
  VAR(Xname.VAR),                                            // XcodeML 7.4.1
  VARDECL(Xname.VAR_DECL),                                   // XcodeML 5.4
  VARLIST(Xname.VAR_LIST),                                   // XcodeML 8.19
  VARREF(Xname.VAR_REF),                                     // XcodeML 7.4.6
  XCODEPROGRAM(Xname.X_CODE_PROGRAM),                        // XcodeML 2

  // Binary expression element
  DIVEXPR(Xname.DIV_EXPR),                                   // XcodeML 7.6
  FCONCATEXPR(Xname.F_CONCAT_EXPR),                          // XcodeML 7.6
  FPOWEREXPR(Xname.F_POWER_EXPR),                            // XcodeML 7.6
  LOGANDEXPR(Xname.LOG_AND_EXPR),                            // XcodeML 7.6
  LOGEQEXPR(Xname.LOG_EQ_EXPR),                              // XcodeML 7.6
  LOGEQVEXPR(Xname.LOG_EQV_EXPR),                            // XcodeML 7.6
  LOGGEEXPR(Xname.LOG_GE_EXPR),                              // XcodeML 7.6
  LOGGTEXPR(Xname.LOG_GT_EXPR),                              // XcodeML 7.6
  LOGLEEXPR(Xname.LOG_LE_EXPR),                              // XcodeML 7.6
  LOGLTEXPR(Xname.LOG_LT_EXPR),                              // XcodeML 7.6
  LOGNEQEXPR(Xname.LOG_NEQ_EXPR),                            // XcodeML 7.6
  LOGNEWVEXPR(Xname.LOG_NEWV_EXPR),                          // XcodeML 7.6
  LOGOREXPR(Xname.LOG_OR_EXPR),                              // XcodeML 7.6
  MINUSEXPR(Xname.MINUS_EXPR),                               // XcodeML 7.6
  MULEXPR(Xname.MUL_EXPR),                                   // XcodeML 7.6
  PLUSEXPR(Xname.PLUS_EXPR),                                 // XcodeML 7.6
  USERBINARYEXPR(Xname.USER_BINARY_EXPR),                    // XcodeML 7.6

  // Unary expression element
  LOGNOTEXPR(Xname.LOG_NOT_EXPR),                            // XcodeML 7.7
  UNARYMINUSEXPR(Xname.UNARY_MINUS_EXPR),                    // XcodeML 7.7
  USERUNARYEXPR(Xname.USER_UNARY_EXPR);                      // XcodeML 7.7

  private final String name;

  Xcode(String s) {
    name = s;
  }

  public String toString() {
    return this.name;
  }

  public static Xcode fromString(String value){
    return Xcode.valueOf(value.toUpperCase());
  }

  /**
   * Get the XcodeML original code.
   * @return XcodeML code.
   */
  public String code(){
    return name;
  }
}
