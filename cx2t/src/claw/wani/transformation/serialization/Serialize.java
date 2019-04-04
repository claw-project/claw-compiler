/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.serialization;

import java.util.ArrayList;
import java.util.List;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.primitive.Function;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
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

/**
 * @author phmarti, havogt, clementval
 */
public class Serialize extends ClawTransformation {

  private Xnode _fctCall;
  private Xnode _anchor;

  private enum SerializationCall {
    SER_ADD_METAINFO, SER_READ, SER_WRITE, SER_READ_PERTURB
  }

  private enum SerializationDirection {
    SER_IN, SER_OUT
  }

  private static final String SER_PPSER_SAVEPOINT = "ppser_savepoint";
  private static final String SER_PPSER_SERIALIZER = "ppser_serializer";
  private static final String SER_PPSER_SERIALIZER_REF = "ppser_serializer_ref";
  private static final String SER_PPSER_ZPERTURB = "ppser_zrperturb";
  private static final String SER_FS_CREATE_SAVEPOINT = "fs_create_savepoint";
  private static final String SER_FS_ADD_SP_METAINFO = "fs_add_savepoint_metainfo";
  private static final String SER_FS_WRITE_FIELD = "fs_write_field";
  private static final String SER_FS_READ_FIELD = "fs_read_field";

  private static final String SER_MODULE_M_SERIALIZE = "m_serialize";
  private static final String SER_MODULE_UTILS_PPSER = "utils_ppser";

  private static final String SAVEPOINT_IN_SUFFIX = "-input";
  private static final String SAVEPOINT_OUT_SUFFIX = "-output";

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
    _anchor = _fctCall.matchAncestor(Xcode.EXPR_STATEMENT) == null
        ? _fctCall.matchAncestor(Xcode.F_ASSIGN_STATEMENT)
        : _fctCall.matchAncestor(Xcode.EXPR_STATEMENT);

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
      throws IllegalTransformationException
  {
    insertUseStatements(xcodeml);
    switch(_claw.getSerModeClauseValue()) {
      case PERTURB:
        perturbIn(xcodeml);
        break;
      case WRITE:
        writeIn(xcodeml);
        break;
      default:
      case READ:
        readIn(xcodeml);
        break;
    }

    writeOut(xcodeml);
    removePragma();
  }

  /**
   * Insert USE statements for the Serialbox modules if not present yet.
   *
   * @param xcodeml Current XcodeML translation unit.
   * @throws IllegalTransformationException If parent function definition
   *                                        cannot be found.
   */
  private void insertUseStatements(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    FfunctionDefinition fctDef = _claw.getPragma().findParentFunction();
    if(fctDef == null) {
      throw new IllegalTransformationException("Cannot find parent function.",
          _claw.getPragma().lineNo());
    }

    if(!fctDef.getDeclarationTable().contains(SER_MODULE_M_SERIALIZE)) {
      fctDef.getDeclarationTable().
          insertUseDecl(xcodeml, SER_MODULE_M_SERIALIZE);
    }
    if(!fctDef.getDeclarationTable().contains(SER_MODULE_UTILS_PPSER)) {
      fctDef.getDeclarationTable().
          insertUseDecl(xcodeml, SER_MODULE_UTILS_PPSER);
    }
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
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT, SER_PPSER_SAVEPOINT, Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, SER_FS_CREATE_SAVEPOINT);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(nameArg);
    serCall.matchDescendant(Xcode.ARGUMENTS).append(savepointArg);

    return serCall;
  }

  private Xnode createMetainfo(XcodeProgram xcodeml, Xnode param)
  {
    Xnode serCall = createSerializeSkeletonFctCall(xcodeml, SerializationCall.SER_ADD_METAINFO);
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);

    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(param.value());
    Xnode varArg = xcodeml.createVar(param.getType(), param.value(), Xscope.GLOBAL);

    arguments.append(nameArg);
    arguments.append(varArg);

    return serCall;
  }

  private List<Xnode> addMetainfo(XcodeProgram xcodeml)
  {
    List<Xnode> params = getParameters(xcodeml);
    List<Xnode> createdNodes = new ArrayList<>();
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(type != null && !type.isArray()
          && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT))
      {
        // TODO save before
        Xnode serCall = createMetainfo(xcodeml, param);
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        exprStmt.insert(serCall);
        createdNodes.add(exprStmt);
      }
    }
    return createdNodes;
  }

  private Xnode createSerializeSkeletonFctCall(XcodeProgram xcodeml,
                                               SerializationCall callType)
  {
    FfunctionType serType = xcodeml.createSubroutineType();
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT,
        SER_PPSER_SAVEPOINT, Xscope.GLOBAL);

    String serFctName;
    switch(callType) {
      case SER_READ:
      case SER_READ_PERTURB:
        serFctName = SER_FS_READ_FIELD;
        break;
      case SER_WRITE:
        serFctName = SER_FS_WRITE_FIELD;
        break;
      case SER_ADD_METAINFO:
        serFctName = SER_FS_ADD_SP_METAINFO;
        break;
      default:
        serFctName = SER_FS_WRITE_FIELD;
    }

    Xnode serCall = xcodeml.createFctCall(serType, serFctName);
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    if(callType == SerializationCall.SER_WRITE) {
      Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,
          SER_PPSER_SERIALIZER, Xscope.GLOBAL);
      arguments.append(serializerArg);
    } else if(callType == SerializationCall.SER_READ || callType == SerializationCall.SER_READ_PERTURB) {
      Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,
          SER_PPSER_SERIALIZER_REF, Xscope.GLOBAL);
      arguments.append(serializerArg);
    }
    arguments.append(savepointArg);
    return serCall;
  }

  /**
   * @param xcodeml
   * @param savepoint
   * @param param
   * @param callType
   * @return exprStmt node created with the specific function call inside.
   */
  private Xnode createSerFctCall(XcodeProgram xcodeml, String savepoint, Xnode param,
                                 SerializationCall callType)
  {
    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepoint + "_" + param.value());
    Xnode varArg =
        xcodeml.createVar(param.getType(), param.value(), Xscope.GLOBAL);
    Xnode serCall = createSerializeSkeletonFctCall(xcodeml, callType);
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    arguments.append(nameArg);
    arguments.append(varArg);
    if(callType == SerializationCall.SER_READ_PERTURB) {
      Xnode perturbArg = xcodeml.createVar(FortranType.REAL,
          SER_PPSER_ZPERTURB, Xscope.GLOBAL);
      arguments.append(perturbArg);
    }
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    exprStmt.insert(serCall);
    return exprStmt;
  }

  private void insertNodes(SerializationDirection direction, List<Xnode> nodes)
  {
    Xnode hook = _anchor;
    for(Xnode node : nodes) {
      if(direction == SerializationDirection.SER_OUT) {
        hook.insertAfter(node);
        hook = node;
      } else if(direction == SerializationDirection.SER_IN) {
        if(hook.equals(_anchor)) {
          hook.insertBefore(node);
        } else {
          hook.insertAfter(node);
        }
        hook = node;
      }
    }
  }

  private void writeIn(XcodeProgram xcodeml)
  {
    List<Xnode> createdNodes = generateCalls(xcodeml,
        SerializationCall.SER_WRITE, SerializationDirection.SER_IN);
    insertNodes(SerializationDirection.SER_IN, createdNodes);
  }

  private void writeOut(XcodeProgram xcodeml)
  {
    List<Xnode> createdNodes = generateCalls(xcodeml,
        SerializationCall.SER_WRITE, SerializationDirection.SER_OUT);
    insertNodes(SerializationDirection.SER_OUT, createdNodes);
  }

  private void readIn(XcodeProgram xcodeml)
  {
    List<Xnode> createdNodes = generateCalls(xcodeml,
        SerializationCall.SER_READ, SerializationDirection.SER_IN);
    insertNodes(SerializationDirection.SER_IN, createdNodes);
  }

  private void perturbIn(XcodeProgram xcodeml)
  {
    List<Xnode> createdNodes = generateCalls(xcodeml,
        SerializationCall.SER_READ_PERTURB, SerializationDirection.SER_IN);
    insertNodes(SerializationDirection.SER_IN, createdNodes);
  }

  private List<Xnode> generateCalls(XcodeProgram xcodeml, SerializationCall callType,
                                    SerializationDirection direction)
  {
    List<Xnode> createdNodes = new ArrayList<>();
    String savepointName;
    if(direction == SerializationDirection.SER_IN) {
      savepointName = _claw.value(ClawClause.SERIALIZE_SAVEPOINT)
          + SAVEPOINT_IN_SUFFIX;
    } else {
      savepointName = _claw.value(ClawClause.SERIALIZE_SAVEPOINT)
          + SAVEPOINT_OUT_SUFFIX;
    }

    Xnode savepoint = createSavepoint(xcodeml, savepointName);
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    exprStmt.insert(savepoint);
    createdNodes.add(exprStmt);
    createdNodes.addAll(addMetainfo(xcodeml));
    if(callType == SerializationCall.SER_WRITE) {
      createdNodes.addAll(writeFields(xcodeml, savepointName,
          direction == SerializationDirection.SER_IN));
    } else if(callType == SerializationCall.SER_READ) {
      createdNodes.addAll(readFields(xcodeml, savepointName));
    } else if(callType == SerializationCall.SER_READ_PERTURB) {
      createdNodes.addAll(perturbFields(xcodeml, savepointName));
    }

    if(direction == SerializationDirection.SER_IN) {
      _anchor.insertBefore(exprStmt);
    } else {
      _anchor.insertAfter(exprStmt);
    }

    return createdNodes;
  }

  private List<Xnode> writeFields(XcodeProgram xcodeml, String savepoint, boolean in) {
    List<Xnode> params = getParameters(xcodeml);
    List<Xnode> createdNodes = new ArrayList<>();
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(in && type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        Xnode exprStmt = createSerFctCall(xcodeml, savepoint, param, SerializationCall.SER_WRITE);
        createdNodes.add(exprStmt);
      }
      if(!in && type.isArray() && (type.getIntent() == Intent.OUT || type.getIntent() == Intent.INOUT)) {
        Xnode exprStmt = createSerFctCall(xcodeml, savepoint, param, SerializationCall.SER_WRITE);
        createdNodes.add(exprStmt);
      }
    }
    return createdNodes;
  }

  private List<Xnode> readFields(XcodeProgram xcodeml, String savepoint) {
    List<Xnode> params = getParameters(xcodeml);
    List<Xnode> createdNodes = new ArrayList<>();
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        Xnode exprStmt = createSerFctCall(xcodeml, savepoint, param, SerializationCall.SER_READ);
        createdNodes.add(exprStmt);
      }
    }
    return createdNodes;
  }

  private List<Xnode> perturbFields(XcodeProgram xcodeml, String savepoint) {
    List<Xnode> params = getParameters(xcodeml);
    List<Xnode> createdNodes = new ArrayList<>();
    for(Xnode param : params) {
      FbasicType type = xcodeml.getTypeTable().getBasicType(param);
      if(type.isArray() && (type.getIntent() == Intent.IN || type.getIntent() == Intent.INOUT)) {
        Xnode exprStmt = createSerFctCall(xcodeml, savepoint, param, SerializationCall.SER_READ_PERTURB);
        createdNodes.add(exprStmt);
      }
    }
    return createdNodes;
  }
}
