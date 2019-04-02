/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.serialization;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;

import static claw.tatsu.xcodeml.xnode.Xname.TYPE_F_VOID;

/**
 * @author phmarti, havogt, clementval
 */
public class Serialize extends ClawTransformation {

  private Xnode _fctCall;

  /**
   * Constructs a new LoopFusion triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop fusion
   *                  transformation.
   */
  public Serialize(ClawPragma directive) {
    super(directive);
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {
    Xnode next = _claw.getPragma().nextSibling();
    if(next == null) {
      xcodeml.addError("Directive is not followed by a valid statement.",
          _claw.getPragma());
      return false;
    }
    if(Xnode.isOfCode(next, Xcode.EXPR_STATEMENT)
        || Xnode.isOfCode(next, Xcode.F_ASSIGN_STATEMENT))
    {
      _fctCall = next.matchSeq(Xcode.FUNCTION_CALL);
      if(_fctCall == null) {
        xcodeml.addError("Function call not found",
            _claw.getPragma());
        return false;
      }
    }
    return true;
  }

  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // Independent transformation
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
  {
    System.out.println(_claw.value(ClawClause.SERIALIZE_SAVEPOINT));
  }

  private Xnode createSavepoint(XcodeProgram xcodeml, String savepoint)
  {
    FfunctionType serType = xcodeml.createFunctionType(xcodeml.getTypeTable().generateHash(FortranType.FUNCTION), TYPE_F_VOID);
    xcodeml.getTypeTable().add(serType);

    // Create the char constant type
    Xnode charType = xcodeml.createBasicType(FortranType.CHARACTER, Intent.NONE);
    Xnode len = xcodeml.createNode(Xcode.LEN);
    len.append(xcodeml.createIntConstant(savepoint.length()));
    charType.append(len);
    xcodeml.getTypeTable().add(charType);

    Xnode serCall = xcodeml.createFctCall(TYPE_F_VOID, "fs_create_savepoint", serType.getType());
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName(savepoint, charType.getType()));
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint",null));

    return serCall;

  }

  private void writeIn(XcodeProgram xcodeml)
  {

  }
}
