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
  ARGUMENTS(Xname.ARGUMENTS),
  ARRAYINDEX(Xname.ARRAY_INDEX),
  FBASICTYPE(Xname.BASIC_TYPE),
  BODY(Xname.BODY),
  CONDITION(Xname.CONDITION),
  DECLARATIONS(Xname.DECLARATIONS),
  FDOSTATEMENT(Xname.F_DO_STATEMENT),
  ELSE(Xname.ELSE),
  EXPRSTATEMENT(Xname.EXPR_STMT),
  FUNCTIONCALL(Xname.FCT_CALL),
  FFUNCTIONDEFINITION(Xname.FCT_DEFINITION),
  FFUNCTIONTYPE(Xname.FCT_TYPE),
  FARRAYREF(Xname.F_ARRAY_REF),
  FASSIGNSTATEMENT(Xname.F_ASSIGN_STMT),
  FCHARACTERREF(Xname.F_CHAR_REF),
  FCOARRAYREF(Xname.F_COARRAY_REF),
  FIFSTATEMENT(Xname.F_IF_STMT),
  FINTCONSTANT(Xname.F_INT_CONST),
  FREALCONSTANT(Xname.F_REAL_CONST),
  FCOMPLEXCONSTANT(Xname.F_COMPLEX_CONST),
  FCHARACTERCONSTANT(Xname.F_CHAR_CONST),
  FLOGICALCONSTANT(Xname.F_LOGICAL_CONST),
  FMEMBERREF(Xname.F_MEMBER_REF),
  FMODULEDEFINITION(Xname.F_MODULE_DEFINITION),
  FSTRUCTTYPE(Xname.F_STRUCT_TYPE),
  GLOBALDECLARATIONS(Xname.GLOBAL_DECLARATIONS),
  GLOBALSYMBOLS(Xname.GLOBAL_SYMBOLS),
  ID(Xname.ID),
  INDEXRANGE(Xname.INDEX_RANGE),
  KIND(Xname.KIND),
  LEN(Xname.LENGTH),
  LOWERBOUND(Xname.LOWER_BOUND),
  NAME(Xname.NAME),
  PARAMS(Xname.PARAMS),
  FPRAGMASTATEMENT(Xname.PRAGMA_STMT),
  STEP(Xname.STEP),
  SYMBOLS(Xname.SYMBOLS),
  THEN(Xname.THEN),
  TYPETABLE(Xname.TYPE_TABLE),
  UPPERBOUND(Xname.UPPER_BOUND),
  VAR(Xname.VAR),
  VARDECL(Xname.VAR_DECL),
  VARREF(Xname.VAR_REF),
  VALUE(Xname.VALUE),
  XCODEPROGRAM(Xname.X_CODE_PROGRAM),

  // Binary expression element
  DIVEXPR(Xname.DIV_EXPR),
  FCONCATEXPR(Xname.F_CONCAT_EXPR),
  FPOWEREXPR(Xname.F_POWER_EXPR),
  LOGANDEXPR(Xname.LOG_AND_EXPR),
  LOGEQEXPR(Xname.LOG_EQ_EXPR),
  LOGEQVEXPR(Xname.LOG_EQV_EXPR),
  LOGGEEXPR(Xname.LOG_GE_EXPR),
  LOGGTEXPR(Xname.LOG_GT_EXPR),
  LOGLEEXPR(Xname.LOG_LE_EXPR),
  LOGLTEXPR(Xname.LOG_LT_EXPR),
  LOGNEQEXPR(Xname.LOG_NEQ_EXPR),
  LOGNEWVEXPR(Xname.LOG_NEWV_EXPR),
  LOGOREXPR(Xname.LOG_OR_EXPR),
  MINUSEXPR(Xname.MINUS_EXPR),
  MULEXPR(Xname.MUL_EXPR),
  PLUSEXPR(Xname.PLUS_EXPR),
  USERBINARYEXPR(Xname.USER_BINARY_EXPR),

  // Unary expression element
  LOGNOTEXPR(Xname.LOG_NOT_EXPR),
  UNARYMINUSEXPR(Xname.UNARY_MINUS_EXPR),
  USERUNARYEXPR(Xname.USER_UNARY_EXPR);

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
