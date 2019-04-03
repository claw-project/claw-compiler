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
import claw.tatsu.xcodeml.xnode.common.Xscope;
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
  private Xnode _anchor;

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

    // Set anchor for new code
    _anchor = _fctCall.matchAncestor(Xcode.EXPR_STATEMENT) == null ? _fctCall.matchAncestor(Xcode.F_ASSIGN_STATEMENT): _fctCall.matchAncestor(Xcode.EXPR_STATEMENT);

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
    readIn(xcodeml);
    perturbIn(xcodeml);

    writeOut(xcodeml);
    removePragma();
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
    Xnode nameArg = xcodeml.createCharConstant(savepoint);
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT, "ppser_savepoint", Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, "fs_create_savepoint");
    serCall.matchDescendant(Xcode.ARGUMENTS).append(nameArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);

    return serCall;
  }

  private Xnode createWriteField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepoint+"_"+param.value());
    Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,"ppser_serializer", Xscope.GLOBAL);
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT,"ppser_savepoint", Xscope.GLOBAL);
    Xnode varArg = xcodeml.createVar(param.getType(), param.value(), Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, "fs_write_field");
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    arguments.append(serializerArg);
    arguments.append(savepointArg);
    arguments.append(nameArg);
    arguments.append(varArg);

    return serCall;
  }

  private Xnode createReadField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepoint + "_" + param.value());
    Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,"ppser_serializer", Xscope.GLOBAL);
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT,"ppser_savepoint", Xscope.GLOBAL);
    Xnode varArg = xcodeml.createVar(param.getType(), param.value(), Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, "fs_read_field");
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    arguments.append(serializerArg);
    arguments.append(savepointArg);
    arguments.append(nameArg);
    arguments.append(varArg);

    return serCall;
  }

  private Xnode createPerturbField(XcodeProgram xcodeml, String savepoint, Xnode param)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepoint+"_"+param.value());
    Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,"ppser_serializer", Xscope.GLOBAL);
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT,"ppser_savepoint", Xscope.GLOBAL);
    Xnode perturbArg = xcodeml.createVar(FortranType.REAL,"ppser_zrperturb", Xscope.GLOBAL);
    Xnode varArg = xcodeml.createVar(param.getType(), param.value(), Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, "fs_read_field");
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    arguments.append(serializerArg);
    arguments.append(savepointArg);
    arguments.append(nameArg);
    arguments.append(varArg);
    arguments.append(perturbArg);

    return serCall;
  }

  private void writeIn(XcodeProgram xcodeml)
  {
    String savename = _claw.value(ClawClause.SERIALIZE_SAVEPOINT) + "-input";
    Xnode savepoint = createSavepoint(xcodeml,
        savename);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    _anchor.insertBefore(exprStmt);
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
    _anchor.insertAfter(exprStmt);

  }

  private void readIn(XcodeProgram xcodeml)
  {
    String savename = _claw.value(ClawClause.SERIALIZE_SAVEPOINT) + "-input";
    Xnode savepoint = createSavepoint(xcodeml,
            savename);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    _anchor.insertBefore(exprStmt);
    exprStmt.insert(savepoint);
    readFields(xcodeml, savename);
  }

  private void perturbIn(XcodeProgram xcodeml)
  {
    String savename = _claw.value(ClawClause.SERIALIZE_SAVEPOINT) + "-input";
    Xnode savepoint = createSavepoint(xcodeml,
            savename);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    _anchor.insertBefore(exprStmt);
    exprStmt.insert(savepoint);
    perturbFields(xcodeml, savename);
  }

  private void writeFields(XcodeProgram xcodeml, String savepoint, boolean in) {
    List<Xnode> params = getParameters(xcodeml);
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(in && type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createWriteField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _anchor.insertBefore(exprStmt);
        exprStmt.insert(serCall);
      }
      if(!in && type.isArray() && (type.getIntent() == Intent.OUT || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createWriteField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _anchor.insertAfter(exprStmt);
        exprStmt.insert(serCall);
      }
    }
  }

  private void readFields(XcodeProgram xcodeml, String savepoint) {
    List<Xnode> params = getParameters(xcodeml);
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createReadField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _anchor.insertBefore(exprStmt);
        exprStmt.insert(serCall);
      }
    }
  }

  private void perturbFields(XcodeProgram xcodeml, String savepoint) {
    List<Xnode> params = getParameters(xcodeml);
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        // TODO save before
        Xnode serCall = createPerturbField(xcodeml, savepoint, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        _anchor.insertBefore(exprStmt);
        exprStmt.insert(serCall);
      }
    }
  }
}
