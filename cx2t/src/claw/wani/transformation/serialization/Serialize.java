/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.serialization;

import java.util.List;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Function;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.Intent;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
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
    writeIn(xcodeml);
    writeOut(xcodeml);
  }

  private List<Xnode> getParameters(XcodeProgram xcodeml) {
    FfunctionDefinition fctDef = Function.findFunctionDefinitionFromFctCall(xcodeml, _fctCall.findParentFunction(), _fctCall);
    FfunctionType fctType = xcodeml.getTypeTable().getFunctionType(fctDef.getType());
    return fctType.getParameters();
  }

  private Xnode createSavepoint(XcodeProgram xcodeml, String savepoint)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode savepointArg = xcodeml.createCharConstant(savepoint);

    Xnode serCall = xcodeml.createFctCall(serType, "fs_create_savepoint");
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint", null));

    return serCall;
  }

  private Xnode createWriteField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode savepointArg = xcodeml.createCharConstant(savepoint+"_"+param.value());

    Xnode serCall = xcodeml.createFctCall(serType, "fs_write_field");
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint", null));

    return serCall;
  }

  private Xnode createReadField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode savepointArg = xcodeml.createCharConstant(savepoint+"_"+param.value());

    Xnode serCall = xcodeml.createFctCall(serType, "fs_read_field");
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint", null));

    return serCall;
  }

  private Xnode createPerturbField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode savepointArg = xcodeml.createCharConstant(savepoint+"_"+param.value());

    Xnode serCall = xcodeml.createFctCall(serType, "fs_read_field");
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint", null));
    serCall.matchDescendant(Xcode.ARGUMENTS).append(xcodeml.createName("ppser_savepoint", null));

    return serCall;
  }

  private void writeIn(XcodeProgram xcodeml)
  {
    String savename = _claw.value(ClawClause.SERIALIZE_SAVEPOINT) + "-input";
    Xnode savepoint = createSavepoint(xcodeml,
        savename);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    _fctCall.ancestor().insertBefore(exprStmt);
    exprStmt.insert(savepoint);
    writeFields(xcodeml, savename, true);
  }

  private void writeOut(XcodeProgram xcodeml)
  {
    String savename = _claw.value(ClawClause.SERIALIZE_SAVEPOINT) + "-output";
    Xnode savepoint = createSavepoint(xcodeml,
        savename);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    exprStmt.insert(savepoint);
    writeFields(xcodeml, savename,false);
    _fctCall.ancestor().insertAfter(exprStmt);

  }

  private void writeFields(XcodeProgram xcodeml, String savepoint, boolean in) {
    List<Xnode> params = getParameters(xcodeml);
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(in && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createWriteField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _fctCall.ancestor().insertBefore(exprStmt);
        exprStmt.insert(serCall);
      }
      if(!in && (type.getIntent() == Intent.OUT || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createWriteField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _fctCall.ancestor().insertAfter(exprStmt);
        exprStmt.insert(serCall);
      }
    }
  }
}
