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
  ARGUMENTS(XelementName.ARGUMENTS),
  ARRAYINDEX(XelementName.ARRAY_INDEX),
  FBASICTYPE(XelementName.BASIC_TYPE),
  BODY(XelementName.BODY),
  CONDITION(XelementName.CONDITION),
  DECLARATIONS(XelementName.DECLARATIONS),
  FDOSTATEMENT(XelementName.F_DO_STATEMENT),
  ELSE(XelementName.ELSE),
  EXPRSTATEMENT(XelementName.EXPR_STMT),
  FUNCTIONCALL(XelementName.FCT_CALL),
  FFUNCTIONDEFINITION(XelementName.FCT_DEFINITION),
  FFUNCTIONTYPE(XelementName.FCT_TYPE),
  FARRAYREF(XelementName.F_ARRAY_REF),
  FASSIGNSTATEMENT(XelementName.F_ASSIGN_STMT),
  FCHARACTERREF(XelementName.F_CHAR_REF),
  FCOARRAYREF(XelementName.F_COARRAY_REF),
  FIFSTATEMENT(XelementName.F_IF_STMT),
  FINTCONSTANT(XelementName.F_INT_CONST),
  FREALCONSTANT(XelementName.F_REAL_CONST),
  FCOMPLEXCONSTANT(XelementName.F_COMPLEX_CONST),
  FCHARACTERCONSTANT(XelementName.F_CHAR_CONST),
  FLOGICALCONSTANT(XelementName.F_LOGICAL_CONST),
  FMEMBERREF(XelementName.F_MEMBER_REF),
  FMODULEDEFINITION(XelementName.F_MODULE_DEFINITION),
  FSTRUCTTYPE(XelementName.F_STRUCT_TYPE),
  GLOBALDECLARATIONS(XelementName.GLOBAL_DECLARATIONS),
  GLOBALSYMBOLS(XelementName.GLOBAL_SYMBOLS),
  ID(XelementName.ID),
  INDEXRANGE(XelementName.INDEX_RANGE),
  KIND(XelementName.KIND),
  LEN(XelementName.LENGTH),
  LOWERBOUND(XelementName.LOWER_BOUND),
  NAME(XelementName.NAME),
  PARAMS(XelementName.PARAMS),
  FPRAGMASTATEMENT(XelementName.PRAGMA_STMT),
  STEP(XelementName.STEP),
  SYMBOLS(XelementName.SYMBOLS),
  THEN(XelementName.THEN),
  TYPETABLE(XelementName.TYPE_TABLE),
  UPPERBOUND(XelementName.UPPER_BOUND),
  VAR(XelementName.VAR),
  VARDECL(XelementName.VAR_DECL),
  VARREF(XelementName.VAR_REF),
  VALUE(XelementName.VALUE),
  XCODEPROGRAM(XelementName.X_CODE_PROGRAM),

  // Binary expression element
  DIVEXPR(XelementName.DIV_EXPR),
  FCONCATEXPR(XelementName.F_CONCAT_EXPR),
  FPOWEREXPR(XelementName.F_POWER_EXPR),
  LOGANDEXPR(XelementName.LOG_AND_EXPR),
  LOGEQEXPR(XelementName.LOG_EQ_EXPR),
  LOGEQVEXPR(XelementName.LOG_EQV_EXPR),
  LOGGEEXPR(XelementName.LOG_GE_EXPR),
  LOGGTEXPR(XelementName.LOG_GT_EXPR),
  LOGLEEXPR(XelementName.LOG_LE_EXPR),
  LOGLTEXPR(XelementName.LOG_LT_EXPR),
  LOGNEQEXPR(XelementName.LOG_NEQ_EXPR),
  LOGNEWVEXPR(XelementName.LOG_NEWV_EXPR),
  LOGOREXPR(XelementName.LOG_OR_EXPR),
  MINUSEXPR(XelementName.MINUS_EXPR),
  MULEXPR(XelementName.MUL_EXPR),
  PLUSEXPR(XelementName.PLUS_EXPR),
  USERBINARYEXPR(XelementName.USER_BINARY_EXPR),

  // Unary expression element
  LOGNOTEXPR(XelementName.LOG_NOT_EXPR),
  UNARYMINUSEXPR(XelementName.UNARY_MINUS_EXPR),
  USERUNARYEXPR(XelementName.USER_UNARY_EXPR);

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
