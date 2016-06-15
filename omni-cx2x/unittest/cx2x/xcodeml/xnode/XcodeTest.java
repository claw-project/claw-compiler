/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xnode;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Test features of Xcode class.
 *
 * @author clementval
 */
public class XcodeTest {
  @Test
  public void stringToEnumTest(){
    assertEnum(Xcode.ARGUMENTS, "arguments");
    assertEnum(Xcode.ARRAYINDEX,"arrayIndex");
    assertEnum(Xcode.FBASICTYPE,"FbasicType");
    assertEnum(Xcode.BODY,"body");
    assertEnum(Xcode.CONDITION,"condition");
    assertEnum(Xcode.DECLARATIONS,"declarations");
    assertEnum(Xcode.FDOSTATEMENT,"FdoStatement");
    assertEnum(Xcode.ELSE,"else");
    assertEnum(Xcode.EXPRSTATEMENT,"exprStatement");
    assertEnum(Xcode.FUNCTIONCALL,"functionCall");
    assertEnum(Xcode.FFUNCTIONDEFINITION,"FfunctionDefinition");
    assertEnum(Xcode.FFUNCTIONTYPE,"FfunctionType");
    assertEnum(Xcode.FARRAYREF,"FarrayRef");
    assertEnum(Xcode.FASSIGNSTATEMENT,"FassignStatement");
    assertEnum(Xcode.FCHARACTERREF,"FcharacterRef");
    assertEnum(Xcode.FCOARRAYREF,"FcoArrayRef");
    assertEnum(Xcode.FIFSTATEMENT,"FifStatement");
    assertEnum(Xcode.FINTCONSTANT,"FintConstant");
    assertEnum(Xcode.FREALCONSTANT,"FrealConstant");
    assertEnum(Xcode.FCOMPLEXCONSTANT,"FcomplexConstant");
    assertEnum(Xcode.FCHARACTERCONSTANT,"FcharacterConstant");
    assertEnum(Xcode.FLOGICALCONSTANT,"FlogicalConstant");
    assertEnum(Xcode.FMEMBERREF,"FmemberRef");
    assertEnum(Xcode.FMODULEDEFINITION,"FmoduleDefinition");
    assertEnum(Xcode.FSTRUCTTYPE,"FstructType");
    assertEnum(Xcode.GLOBALDECLARATIONS,"globalDeclarations");
    assertEnum(Xcode.GLOBALSYMBOLS,"globalSymbols");
    assertEnum(Xcode.ID,"id");
    assertEnum(Xcode.INDEXRANGE,"indexRange");
    assertEnum(Xcode.KIND,"kind");
    assertEnum(Xcode.LEN,"len");
    assertEnum(Xcode.LOWERBOUND,"lowerBound");
    assertEnum(Xcode.NAME,"name");
    assertEnum(Xcode.PARAMS,"params");
    assertEnum(Xcode.FPRAGMASTATEMENT,"FpragmaStatement");
    assertEnum(Xcode.STEP,"step");
    assertEnum(Xcode.SYMBOLS,"symbols");
    assertEnum(Xcode.THEN,"then");
    assertEnum(Xcode.TYPETABLE,"typeTable");
    assertEnum(Xcode.UPPERBOUND,"upperBound");
    assertEnum(Xcode.VAR,"Var");
    assertEnum(Xcode.VARDECL,"varDecl");
    assertEnum(Xcode.VARREF,"varRef");
    assertEnum(Xcode.VALUE,"value");
    assertEnum(Xcode.XCODEPROGRAM,"XcodeProgram");
    assertEnum(Xcode.DIVEXPR,"divExpr");
    assertEnum(Xcode.FCONCATEXPR,"FconcatExpr");
    assertEnum(Xcode.FPOWEREXPR,"FpowerExpr");
    assertEnum(Xcode.LOGANDEXPR,"logAndExpr");
    assertEnum(Xcode.LOGEQEXPR,"logEQExpr");
    assertEnum(Xcode.LOGEQVEXPR,"logEQVExpr");
    assertEnum(Xcode.LOGGEEXPR,"logGEExpr");
    assertEnum(Xcode.LOGGTEXPR,"logGTExpr");
    assertEnum(Xcode.LOGLEEXPR,"logLEExpr");
    assertEnum(Xcode.LOGLTEXPR,"logLTExpr");
    assertEnum(Xcode.LOGNEQEXPR,"logNEQExpr");
    assertEnum(Xcode.LOGNEWVEXPR,"logNEWVExpr");
    assertEnum(Xcode.LOGOREXPR,"logOrExpr");
    assertEnum(Xcode.MINUSEXPR,"minusExpr");
    assertEnum(Xcode.MULEXPR,"mulExpr");
    assertEnum(Xcode.PLUSEXPR,"plusExpr");
    assertEnum(Xcode.USERBINARYEXPR,"userBinaryExpr");
    assertEnum(Xcode.LOGNOTEXPR,"logNotExpr");
    assertEnum(Xcode.UNARYMINUSEXPR,"unaryMinusExpr");
    assertEnum(Xcode.USERUNARYEXPR,"userUnaryExpr");
  }

  private void assertEnum(Xcode opcode, String value){
    Xcode ret = Xcode.fromString(value);
    assertEquals(opcode, ret);
  }
}
