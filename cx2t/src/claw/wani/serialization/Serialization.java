/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.serialization;

import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;

import java.util.ArrayList;
import java.util.List;

/**
 * Helper class to insert serialization call in XcodeML/F
 *
 * @author clementval
 */
public class Serialization {

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

  private static final String SAVEPOINT_IN_SUFFIX = "_in";
  private static final String SAVEPOINT_OUT_SUFFIX = "_out";

  // Avoid potential instantiation of this class
  private Serialization() {
  }

  private enum SerializationCall {
    SER_ADD_METAINFO, SER_READ, SER_WRITE, SER_READ_PERTURB
  }

  private static Xnode createSavepoint(XcodeProgram xcodeml,
                                       String savepointName)
  {
    FfunctionType serType = xcodeml.createSubroutineType();

    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepointName);
    Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT,
        SER_PPSER_SAVEPOINT, Xscope.GLOBAL);

    Xnode serCall = xcodeml.createFctCall(serType, SER_FS_CREATE_SAVEPOINT);
    serCall.matchDescendant(Xcode.ARGUMENTS)
        .append(nameArg).append(savepointArg);

    return serCall;
  }

  /**
   * Create a write field function call to the serialization library.
   *
   * @param xcodeml       Current XcodeML translation unit.
   * @param savepointName Name of the savepoint.
   * @param fieldName     Name of the field.
   * @return Newly created exprStmt node encapsulating the function call.
   */
  private static Xnode createWriteFieldCall(XcodeProgram xcodeml,
                                            String savepointName,
                                            String fieldName)
  {
    return createSerFctCall(xcodeml, savepointName, fieldName,
        SerializationCall.SER_WRITE);
  }

  /**
   * Create the skeletion of a function call for serialization functions.
   *
   * @param xcodeml  Current XcodeML translation unit.
   * @param callType Type of call for the
   * @return Newly create functionCall node.
   */
  private static Xnode createSerSkeletonFctCall(XcodeProgram xcodeml,
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
      case SER_ADD_METAINFO:
        serFctName = SER_FS_ADD_SP_METAINFO;
        break;
      case SER_WRITE:
      default:
        serFctName = SER_FS_WRITE_FIELD;
    }

    Xnode serCall = xcodeml.createFctCall(serType, serFctName);
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    if(callType == SerializationCall.SER_WRITE) {
      Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,
          SER_PPSER_SERIALIZER, Xscope.GLOBAL);
      arguments.append(serializerArg);
    } else if(callType == SerializationCall.SER_READ
        || callType == SerializationCall.SER_READ_PERTURB)
    {
      Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT,
          SER_PPSER_SERIALIZER_REF, Xscope.GLOBAL);
      arguments.append(serializerArg);
    }
    arguments.append(savepointArg);
    return serCall;
  }

  /**
   * @param xcodeml       Current XcodeML translation unit.
   * @param savepointName Name of the savepoint (will be part of the serialized
   *                      name)
   * @param fieldName     Name of the field (will be part of the serialized
   *                      name)
   * @param callType      Type of serialization call from the enum.
   * @return exprStmt node created with the specific function call inside.
   */
  private static Xnode createSerFctCall(XcodeProgram xcodeml,
                                        String savepointName,
                                        String fieldName,
                                        SerializationCall callType)
  {
    // Create the char constant type
    Xnode nameArg = xcodeml.createCharConstant(savepointName + "_" + fieldName);
    Xnode varArg = xcodeml.createVar(FortranType.REAL, fieldName, Xscope.GLOBAL);
    Xnode serCall = createSerSkeletonFctCall(xcodeml, callType);
    Xnode arguments = serCall.matchDescendant(Xcode.ARGUMENTS);
    arguments.append(nameArg).append(varArg);
    if(callType == SerializationCall.SER_READ_PERTURB) {
      Xnode perturbArg = xcodeml.createVar(FortranType.REAL, SER_PPSER_ZPERTURB,
          Xscope.GLOBAL);
      arguments.append(perturbArg);
    }
    Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
    exprStmt.insert(serCall);
    return exprStmt;
  }

  /**
   * Create function calls to the serialization library to write a savepoint.
   *
   * @param xcodeml       Current XcodeML translation unit.
   * @param hook          Hook for node insertion.
   * @param fields        List of fields to be written.
   * @param savepointName Name of the savepoint.
   * @return Last inserted node.
   */
  public static Xnode writeSavepoint(XcodeProgram xcodeml, Xnode hook,
                                     List<String> fields, String savepointName,
                                     SerializationStep step)
  {
    savepointName = String.format("%s_%s", savepointName,
        step == SerializationStep.SER_IN
            ? SAVEPOINT_IN_SUFFIX : SAVEPOINT_OUT_SUFFIX);

    List<Xnode> nodes = new ArrayList<>();
    nodes.add(xcodeml.createNode(Xcode.EXPR_STATEMENT)
        .insert(createSavepoint(xcodeml, savepointName)));

    for(String fieldName : fields) {
      nodes.add(createWriteFieldCall(xcodeml, savepointName, fieldName));
    }

    return insertNodes(step, hook, nodes);
  }

  /**
   * Insert nodes for an input or output serialization.
   *
   * @param step  Serialization step information.
   * @param hook  Hook node to start insertion.
   * @param nodes List of nodes to be inserted.
   */
  private static Xnode insertNodes(SerializationStep step, Xnode hook,
                                   List<Xnode> nodes)
  {
    Xnode crtHook = hook;
    for(Xnode node : nodes) {
      if(step == SerializationStep.SER_OUT) {
        crtHook.insertAfter(node);
        crtHook = node;
      } else if(step == SerializationStep.SER_IN) {
        if(crtHook.equals(hook)) {
          crtHook.insertBefore(node);
        } else {
          crtHook.insertAfter(node);
        }
        crtHook = node;
      }
    }
    return crtHook;
  }

  /**
   * Insert the correct USE statements for using the serialization library.
   *
   * @param xcodeml Current XcodeML/F translation unit.
   * @param fctDef  Function definition.
   */
  public static void insertImports(XcodeProgram xcodeml,
                                   FfunctionDefinition fctDef)
  {
    fctDef.getDeclarationTable().insertUseDecl(xcodeml, SER_MODULE_M_SERIALIZE);
    fctDef.getDeclarationTable().insertUseDecl(xcodeml, SER_MODULE_UTILS_PPSER);
  }

}
