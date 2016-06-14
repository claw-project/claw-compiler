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
  ARGUMENTS("arguments"),
  ARRAYINDEX("arrayIndex"),
  FBASICTYPE("FbasicType"),
  BODY("body"),
  CONDITION("condition"),
  DECLARATIONS("declarations"),
  FDOSTATEMENT("FdoStatement"),
  ELSE("else"),
  EXPRSTATEMENT("exprStatement"),
  FUNCTIONCALL("functionCall"),
  FFUNCTIONDEFINITION("FfunctionDefinition"),
  FFUNCTIONTYPE("FfunctionType"),
  FARRAYREF("FarrayRef"),
  FASSIGNSTATEMENT("FassignStatement"),
  FCHARACTERREF("FcharacterRef"),
  FCOARRAYREF("FcoArrayRef"),
  FIFSTATEMENT("FifStatement"),
  FINTCONSTANT("FintConstant"),
  FREALCONSTANT("FrealConstant"),
  FCOMPLEXCONSTANT("FcomplexConstant"),
  FCHARACTERCONSTANT("FcharacterConstant"),
  FLOGICALCONSTANT("FlogicalConstant"),
  FMEMBERREF("FmemberRef"),
  FMODULEDEFINITION("FmoduleDefinition"),
  FSTRUCTTYPE("FstructType"),
  GLOBALDECLARATIONS("globalDeclarations"),
  GLOBALSYMBOLS("globalSymbols"),
  ID("id"),
  INDEXRANGE("indexRange"),
  KIND("kind"),
  LEN("len"),
  LOWERBOUND("lowerBound"),
  NAME("name"),
  PARAMS("params"),
  FPRAGMASTATEMENT("FpragmaStatement"),
  STEP("step"),
  SYMBOLS("symbols"),
  THEN("then"),
  TYPETABLE("typeTable"),
  UPPERBOUND("upperBound"),
  VAR("Var"),
  VARDECL("varDecl"),
  VARREF("varRef"),
  VALUE("value"),
  XCODEPROGRAM("XcodeProgram"),

  // Binary expression element
  DIVEXPR("divExpr"),
  FCONCATEXPR("FconcatExpr"),
  FPOWEREXPR("FpowerExpr"),
  LOGANDEXPR("logAndExpr"),
  LOGEQEXPR("logEQExpr"),
  LOGEQVEXPR("logEQVExpr"),
  LOGGEEXPR("logGEExpr"),
  LOGGTEXPR("logGTExpr"),
  LOGLEEXPR("logLEExpr"),
  LOGLTEXPR("logLTExpr"),
  LOGNEQEXPR("logNEQExpr"),
  LOGNEWVEXPR("logNEWVExpr"),
  LOGOREXPR("logOrExpr"),
  MINUSEXPR("minusExpr"),
  MULEXPR("mulExpr"),
  PLUSEXPR("plusExpr"),
  USERBINARYEXPR("userBinaryExpr"),

  // Unary expression element
  LOGNOTEXPR("logNotExpr"),
  UNARYMINUSEXPR("unaryMinusExpr"),
  USERUNARYEXPR("userUnaryExpr");

  private final String name;

  Xcode(String s) {
    name = s;
  }

  /**
   * Get the XcodeML original code.
   * @return XcodeML code.
   */
  public String code(){
    return name;
  }

  public String toString() {
    return this.name;
  }

  public static Xcode fromString(String value){
    return Xcode.valueOf(value.toUpperCase());
  }
}
