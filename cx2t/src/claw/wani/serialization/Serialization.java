/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.serialization;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import claw.tatsu.xcodeml.abstraction.FunctionCall;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.Xscope;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;
import claw.tatsu.xcodeml.xnode.fortran.FortranType;
import claw.wani.x2t.configuration.Configuration;

/**
 * Helper class to insert serialization call in XcodeML/F
 *
 * @author clementval
 */
public class Serialization
{

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

    private static final String SAVEPOINT_IN_SUFFIX = "in";
    private static final String SAVEPOINT_OUT_SUFFIX = "out";

    // Avoid potential instantiation of this class
    private Serialization()
    {
    }

    private enum SerializationCall {
        SER_ADD_METAINFO, SER_READ, SER_WRITE, SER_READ_PERTURB
    }

    /**
     * Create function call to fs_create_savepoint
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param savepointName Name of the savepoint.
     * @return Newly create functionCall node.
     */
    private static Xnode createSavepoint(XcodeProgram xcodeml, String savepointName)
    {
        FfunctionType serType = xcodeml.createSubroutineType();

        // Create the char constant type
        Xnode nameArg = xcodeml.createCharConstant(savepointName);
        Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT, SER_PPSER_SAVEPOINT, Xscope.GLOBAL);

        Xnode serCall = xcodeml.createFctCall(serType, SER_FS_CREATE_SAVEPOINT);
        serCall.matchDescendant(Xcode.ARGUMENTS).append(nameArg).append(savepointArg);

        return xcodeml.createNode(Xcode.EXPR_STATEMENT).insert(serCall);
    }

    /**
     * Create a write field function call to the serialization library.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param savepointName Name of the savepoint.
     * @param field         Representation of the field.
     * @return Newly created exprStmt node encapsulating the function call.
     */
    private static Xnode createWriteFieldCall(XcodeProgram xcodeml, String savepointName, String field,
            String fieldName)
    {
        return createReadWriteFctCall(xcodeml, savepointName, field, fieldName, SerializationCall.SER_WRITE);
    }

    /**
     * Create a read field function call to the serialization library.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param savepointName Name of the savepoint.
     * @param field         Representation of the field.
     * @return Newly created exprStmt node encapsulating the function call.
     */
    private static Xnode createReadFieldCall(XcodeProgram xcodeml, String savepointName, String field, String fieldName)
    {
        return createReadWriteFctCall(xcodeml, savepointName, field, fieldName, SerializationCall.SER_READ);
    }

    /**
     * Create the skeletion of a function call for serialization functions.
     *
     * @param xcodeml  Current XcodeML translation unit.
     * @param callType Type of call for the
     * @return Newly create functionCall node.
     */
    private static FunctionCall createBaseSerFctCall(XcodeProgram xcodeml, SerializationCall callType)
    {
        FfunctionType serType = xcodeml.createSubroutineType();
        Xnode savepointArg = xcodeml.createVar(FortranType.STRUCT, SER_PPSER_SAVEPOINT, Xscope.GLOBAL);

        String serFctName;
        switch (callType)
        {
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

        FunctionCall serCall = xcodeml.createFctCall(serType, serFctName);

        if (callType == SerializationCall.SER_WRITE)
        {
            Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT, SER_PPSER_SERIALIZER, Xscope.GLOBAL);
            serCall.addArguments(serializerArg);
        } else if (callType == SerializationCall.SER_READ || callType == SerializationCall.SER_READ_PERTURB)
        {
            Xnode serializerArg = xcodeml.createVar(FortranType.STRUCT, SER_PPSER_SERIALIZER_REF, Xscope.GLOBAL);
            serCall.addArguments(serializerArg);
        }
        serCall.addArguments(savepointArg);
        return serCall;
    }

    /**
     * @param xcodeml       Current XcodeML translation unit.
     * @param savepointName Name of the savepoint (will be part of the serialized
     *                      name)
     * @param field         Representation of the field.
     * @param callType      Type of serialization call from the enum.
     * @return exprStmt node created with the specific function call inside.
     */
    private static Xnode createReadWriteFctCall(XcodeProgram xcodeml, String savepointName, String field,
            String fieldName, SerializationCall callType)
    {
        // Create the char constant type
        Xnode nameArg = xcodeml.createCharConstant(savepointName + "_" + fieldName);
        Xnode varArg = xcodeml.createVar(FortranType.REAL, field, Xscope.GLOBAL);
        FunctionCall serCall = createBaseSerFctCall(xcodeml, callType);
        serCall.addArguments(nameArg);
        serCall.addArguments(varArg);
        if (callType == SerializationCall.SER_READ_PERTURB)
        {
            Xnode perturbArg = xcodeml.createVar(FortranType.REAL, SER_PPSER_ZPERTURB, Xscope.GLOBAL);
            serCall.addArguments(perturbArg);
        }
        Xnode exprStmt = xcodeml.createNode(Xcode.EXPR_STATEMENT);
        exprStmt.insert(serCall);
        return exprStmt;
    }

    /**
     * Create a fs_add_savepoint_metainfo call to the serialization library.
     *
     * @param xcodeml Current XcodeML translation unit.
     * @param key     Metadata key.
     * @param value   Metadata value.
     * @return exprStmt node created with the specific function call inside.
     */
    private static Xnode createAddMetaInfoCall(XcodeProgram xcodeml, String key, String value)
    {
        FunctionCall serCall = createBaseSerFctCall(xcodeml, SerializationCall.SER_ADD_METAINFO);

        // Create the char constant type
        Xnode metadataName = xcodeml.createCharConstant(key);
        serCall.addArguments(metadataName);

        if (value.contains("%"))
        {
            String[] values = value.split("%");
            serCall.addArguments(xcodeml.createNode(Xcode.F_MEMBER_REF).setAttribute(Xattr.MEMBER, values[1]).append(
                    xcodeml.createNode(Xcode.VAR_REF).append(xcodeml.createNode(Xcode.VAR).setValue(values[0]))));
        } else
        {
            serCall.addArguments(xcodeml.createNode(Xcode.VAR).setValue(value));
        }

        return xcodeml.createNode(Xcode.EXPR_STATEMENT).insert(serCall);
    }

    /**
     * Create function calls to the serialization library to write a savepoint.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param hook          Hook for node insertion.
     * @param metadata      Key=value information for metadata.
     * @param fields        List of fields to be written.
     * @param savepointName Name of the savepoint.
     * @return Last inserted node.
     */
    public static Xnode generateWriteSavepoint(Configuration cfg, XcodeProgram xcodeml, Xnode hook,
            Map<String, String> metadata, List<String> fields, String savepointName, SerializationStep step)
    {
        return generateSavepoint(cfg, xcodeml, hook, metadata, fields, savepointName, step, SerializationMode.WRITE);
    }

    /**
     * Create function calls to the serialization library to read a savepoint.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param hook          Hook for node insertion.
     * @param metadata      Key=value information for metadata.
     * @param fields        List of fields to be written.
     * @param savepointName Name of the savepoint.
     * @return Last inserted node.
     */
    public static Xnode generateReadSavepoint(Configuration cfg, XcodeProgram xcodeml, Xnode hook,
            Map<String, String> metadata, List<String> fields, String savepointName, SerializationStep step)
    {
        return generateSavepoint(cfg, xcodeml, hook, metadata, fields, savepointName, step, SerializationMode.READ);
    }

    /**
     * Create function calls to the serialization library to write or read a
     * savepoint.
     *
     * @param xcodeml       Current XcodeML translation unit.
     * @param hook          Hook for node insertion.
     * @param metadata      Key=value information for metadata.
     * @param fields        List of fields.
     * @param savepointName Name of the savepoint.
     * @return Last inserted node.
     */
    private static Xnode generateSavepoint(Configuration cfg, XcodeProgram xcodeml, Xnode hook,
            Map<String, String> metadata, List<String> fields, String savepointName, SerializationStep step,
            SerializationMode mode)
    {
        if (!cfg.getBooleanParameter(Configuration.SCA_SERIALIZATION_ENABLED))
        {
            return hook;
        }

        if ((cfg.seriliazeRead() && mode != SerializationMode.READ)
                || (cfg.seriliazeWrite() && mode != SerializationMode.WRITE))
        {
            return hook;
        }

        savepointName = String.format("%s_%s", savepointName,
                step == SerializationStep.SER_IN ? SAVEPOINT_IN_SUFFIX : SAVEPOINT_OUT_SUFFIX);

        List<Xnode> nodes = new ArrayList<>();
        nodes.add(createSavepoint(xcodeml, savepointName));

        for (Map.Entry<String, String> entry : metadata.entrySet())
        {
            nodes.add(createAddMetaInfoCall(xcodeml, entry.getKey(), entry.getValue()));
        }

        Set<String> uniqueFields = new HashSet<>(fields);
        Map<String, Integer> fieldNames = new HashMap<>();
        for (String field : uniqueFields)
        {
            String fieldName = cleanUpFieldName(field);
            if (fieldNames.containsKey(fieldName))
            {
                int counter = fieldNames.get(fieldName) + 1;
                fieldNames.replace(fieldName, counter);
                fieldName = String.format("%s_%d", fieldName, counter);
            } else
            {
                fieldNames.put(fieldName, 0);
            }

            if (mode == SerializationMode.WRITE)
            {
                nodes.add(createWriteFieldCall(xcodeml, savepointName, field, fieldName));
            } else if (mode == SerializationMode.READ)
            {
                nodes.add(createReadFieldCall(xcodeml, savepointName, field, fieldName));
            }
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
    private static Xnode insertNodes(SerializationStep step, Xnode hook, List<Xnode> nodes)
    {
        Xnode crtHook = hook;
        for (Xnode node : nodes)
        {
            if (step == SerializationStep.SER_OUT)
            {
                crtHook.insertAfter(node);
                crtHook = node;
            } else if (step == SerializationStep.SER_IN)
            {
                if (crtHook.equals(hook))
                {
                    crtHook.insertBefore(node);
                } else
                {
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
    public static void insertImports(Configuration cfg, XcodeProgram xcodeml, FfunctionDefinition fctDef)
    {
        if (!cfg.getBooleanParameter(Configuration.SCA_SERIALIZATION_ENABLED))
        {
            return;
        }
        fctDef.getDeclarationTable().insertUseDecl(xcodeml, SER_MODULE_M_SERIALIZE);
        fctDef.getDeclarationTable().insertUseDecl(xcodeml, SER_MODULE_UTILS_PPSER);
    }

    /**
     * Remove illegal character in the field name.
     *
     * @param fieldName Original field name.
     * @return Cleaned up field name.
     */
    private static String cleanUpFieldName(String fieldName)
    {
        return fieldName.replaceAll("%", "_").replaceAll("\\(.*\\)", "").replaceAll(":", "").replaceAll(",", "");
    }

}
