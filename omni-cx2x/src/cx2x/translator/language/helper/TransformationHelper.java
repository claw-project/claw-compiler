/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.config.Configuration;
import cx2x.translator.language.base.ClawDirective;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.common.ClawDimension;
import cx2x.translator.language.common.ClawReshapeInfo;
import cx2x.translator.language.common.OverPosition;
import cx2x.translator.language.helper.accelerator.AcceleratorDirective;
import cx2x.translator.language.helper.target.Target;
import cx2x.translator.transformation.claw.parallelize.PromotionInfo;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.translator.transformation.loop.LoopInterchange;
import cx2x.translator.xnode.ClawAttr;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.*;
import exc.xcodeml.XcodeMLtools_Fmod;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import xcodeml.util.XmOption;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * The class TransformationHelper contains only static method to help the
 * generation of additional transformation described as clause in main
 * directive.
 *
 * @author clementval
 */
public class TransformationHelper {


  /**
   * Generate corresponding additional transformation according to optional
   * clauses given to the directive.
   *
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param xcodeml     Current XcodeML program.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached.
   */
  public static void generateAdditionalTransformation(ClawLanguage claw,
                                                      XcodeProgram xcodeml,
                                                      Transformer transformer,
                                                      Xnode stmt)
  {
    // Order doesn't matter
    applyFusionClause(claw, transformer, stmt);
    applyInterchangeClause(claw, xcodeml, transformer, stmt);
  }


  /**
   * Generate loop fusion transformation if the clause is present in the
   * directive.
   *
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached. Must
   *                    be a FdoStatement for the loop fusion transformation.
   */
  private static void applyFusionClause(ClawLanguage claw,
                                        Transformer transformer,
                                        Xnode stmt)
  {
    if(claw.hasFusionClause() && stmt.opcode() == Xcode.FDOSTATEMENT) {
      ClawLanguage l = ClawLanguage.createLoopFusionLanguage(claw);
      LoopFusion fusion = new LoopFusion(stmt, l);
      // TODO maybe run analysis
      transformer.addTransformation(fusion);

      if(XmOption.isDebugOutput()) {
        System.out.println("Loop fusion added: " + claw.getGroupValue());
      }
    }
  }

  /**
   * Generate loop interchange transformation if the clause is present in the
   * directive.
   *
   * @param claw        ClawLanguage object that tells encapsulates all
   *                    information about the current directives and its
   *                    clauses.
   * @param xcodeml     Current XcodeML program.
   * @param transformer Transformer object in which new transformation are
   *                    added.
   * @param stmt        Statement on which the transformation is attached. Must
   *                    be a FdoStatement for the loop interchange
   *                    transformation.
   */
  private static void applyInterchangeClause(ClawLanguage claw,
                                             XcodeProgram xcodeml,
                                             Transformer transformer,
                                             Xnode stmt)
  {
    if(claw.hasInterchangeClause() && stmt.opcode() == Xcode.FDOSTATEMENT) {
      Xnode p = new Xnode(Xcode.FPRAGMASTATEMENT, xcodeml);
      stmt.insertBefore(p);
      ClawLanguage l = ClawLanguage.createLoopInterchangeLanguage(claw, p);
      LoopInterchange interchange = new LoopInterchange(l);
      transformer.addTransformation(interchange);
      if(XmOption.isDebugOutput()) {
        System.out.println("Loop interchange added: " + claw.getIndexes());
      }
    }
  }

  /**
   * Find the id element in the current function definition or in parent if
   * nested.
   *
   * @param fctDef Current function definition.
   * @param name   Id name to be searched for.
   * @return The element if found. Null otherwise.
   */
  private static Xid getIdInNestedFctDef(XfunctionDefinition fctDef,
                                         String name)
  {
    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getSymbolTable().get(name);
    }
    XfunctionDefinition upperDef = XnodeUtil.findParentFunction(fctDef);
    if(upperDef == null) {
      return null;
    }
    return getIdInNestedFctDef(upperDef, name);
  }

  /**
   * Find the declaration element in the current function definition or in
   * parent if nested.
   *
   * @param fctDef Current function definition.
   * @param name   Declaration name to be searched for.
   * @return The element if found. Null otherwise.
   */
  private static Xdecl getDeclInNestedFctDef(XfunctionDefinition fctDef,
                                             String name)
  {
    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getDeclarationTable().get(name);
    }
    XfunctionDefinition upperDef = XnodeUtil.findParentFunction(fctDef);
    if(upperDef == null) {
      return null;
    }
    return getDeclInNestedFctDef(upperDef, name);
  }


  /**
   * Apply the reshape clause transformation.
   *
   * @param claw    The claw language object holding the reshape information.
   * @param xcodeml The current XcodeML program.
   * @throws IllegalTransformationException when reshape information are not
   *                                        valid or an new element cannot be
   *                                        created.
   */
  public static void applyReshapeClause(ClawLanguage claw,
                                        XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(!claw.hasReshapeClause()) {
      return;
    }
    XfunctionDefinition fctDef =
        XnodeUtil.findParentFunction(claw.getPragma());
    if(fctDef == null) {
      throw new IllegalTransformationException("Cannot apply reshape clause." +
          "Parent function definition not found.", claw.getPragma().lineNo());
    }

    for(ClawReshapeInfo reshapeInfo : claw.getReshapeClauseValues()) {
      Xid id = getIdInNestedFctDef(fctDef, reshapeInfo.getArrayName());
      Xdecl decl = getDeclInNestedFctDef(fctDef, reshapeInfo.getArrayName());

      if(id == null || decl == null) {
        throw new IllegalTransformationException("Cannot apply reshape clause."
            + "Variable " + reshapeInfo.getArrayName() + " not found in " +
            "declaration table.", claw.getPragma().lineNo());
      }

      String crtTypeHash = id.getType();

      Xtype rawType = xcodeml.getTypeTable().get(crtTypeHash);
      if(!(rawType instanceof XbasicType)) {
        throw new IllegalTransformationException(
            String.format("Reshape variable %s is not a basic type.",
                reshapeInfo.getArrayName()),
            claw.getPragma().lineNo()
        );
      }
      XbasicType crtType = (XbasicType) rawType;

      // Check dimension
      if(crtType.getDimensions() < reshapeInfo.getTargetDimension()) {
        throw new IllegalTransformationException(
            String.format(
                "Reshape variable %s has smaller dimension than requested.",
                reshapeInfo.getArrayName()
            ), claw.getPragma().lineNo()
        );
      }

      // Create new type
      XbasicType newType = crtType.cloneNode();
      newType.setType(xcodeml.getTypeTable().generateRealTypeHash());
      if(reshapeInfo.getTargetDimension() == 0) { // Demote to scalar
        newType.resetDimension();
      } else { // Demote to smaller dimension array

        if(crtType.getDimensions() - reshapeInfo.getKeptDimensions().size() !=
            reshapeInfo.getTargetDimension())
        {
          throw new IllegalTransformationException(
              String.format("Reshape information for %s not valid. " +
                      "Target dimension and kept dimension mismatch.",
                  reshapeInfo.getArrayName())
          );
        }
        newType.removeDimension(reshapeInfo.getKeptDimensions());
      }
      xcodeml.getTypeTable().add(newType);

      // Update symbol & declaration
      id.setType(newType.getType());
      decl.matchSeq(Xcode.NAME).setAttribute(Xattr.TYPE, newType.getType());

      // Update array references
      List<Xnode> refs =
          XnodeUtil.getAllArrayReferences(fctDef.body(),
              reshapeInfo.getArrayName());

      for(Xnode ref : refs) {
        if(reshapeInfo.getTargetDimension() == 0) {
          XnodeUtil.demoteToScalar(ref);
        } else {
          XnodeUtil.demote(ref, reshapeInfo.getKeptDimensions());
        }
      }
    }
  }

  /**
   * Locate a module file generated by CLAW translator.
   *

   * @param modName Name of the module.
   * @return A Xmod object representing the module if found. Null otherwise.
   */
  public static Xmod locateClawModuleFile(Target target,
                                          AcceleratorDirective directive,
                                          String modName)
  {
    String modPrefix = Utility.formattedModuleFilePrefix(target, directive);
    for(String dir : XcodeMLtools_Fmod.getSearchPath()) {
      String path = dir + "/" + modName + modPrefix +
          XnodeUtil.XMOD_FILE_EXTENSION;
      File f = new File(path);
      if(f.exists()) {
        Document doc = XnodeUtil.readXmlFile(path);
        return doc != null ? new Xmod(doc, modName, dir) : null;
      }
    }
    return null;
  }

  /**
   * Update the function signature in the module file to reflects local changes.
   *
   * @param xcodeml     Current XcodeML file unit.
   * @param fctDef      Function definition that has been changed.
   * @param fctType     Function type that has been changed.
   * @param modDef      Module definition holding the function definition.
   * @param claw        Pragma that has triggered the transformation.
   * @param transformer Current transformer object.
   * @throws IllegalTransformationException If the module file or the function
   *                                        cannot be located
   */
  public static void updateModuleSignature(XcodeProgram xcodeml,
                                           XfunctionDefinition fctDef,
                                           XfunctionType fctType,
                                           XmoduleDefinition modDef,
                                           ClawLanguage claw,
                                           cx2x.xcodeml.transformation.
                                               Transformer transformer,
                                           boolean importFctType)
      throws IllegalTransformationException
  {
    Xmod mod;
    if(transformer.getModCache().isModuleLoaded(modDef.getName())) {
      mod = transformer.getModCache().get(modDef.getName());
    } else {
      mod = fctDef.findContainingModule();
      if(mod == null) {
        throw new IllegalTransformationException(
            "Unable to locate module file for: " + modDef.getName(),
            claw.getPragma().lineNo());
      }
      transformer.getModCache().add(modDef.getName(), mod);
    }

    XfunctionType fctTypeMod;
    if(importFctType) {
      Node rawNode = mod.getDocument().importNode(fctType.element(), true);
      mod.getTypeTable().element().appendChild(rawNode);
      XfunctionType importedFctType = new XfunctionType((Element) rawNode);
      Xid importedFctTypeId = mod.createId(importedFctType.getType(),
          Xname.SCLASS_F_FUNC, fctDef.getName().value());
      mod.getIdentifiers().add(importedFctTypeId);

      // check if params need to be imported as well
      if(importedFctType.getParameterNb() > 0) {
        for(Xnode param : importedFctType.getParams().getAll()) {
          mod.importType(xcodeml, param.getAttribute(Xattr.TYPE));
        }
      }
      return;
    } else {
      fctTypeMod = (XfunctionType) mod.getTypeTable().get(
          fctDef.getName().getAttribute(Xattr.TYPE));
    }

    if(fctTypeMod == null) {
      /* Workaround for a bug in OMNI Compiler. Look at test case
       * claw/abstraction12. In this test case, the XcodeML/F intermediate
       * representation for the function call points to a FfunctionType element
       * with no parameters. Thus, we have to matchSeq the correct FfunctionType
       * for the same function/subroutine with the same name in the module
       * symbol table. */
      String errorMsg = "Unable to locate fct " + fctDef.getName().value() +
          " in module " + modDef.getName();
      int lineNo = claw.getPragma().lineNo();

      // If not, try to matchSeq the correct FfunctionType in the module definitions
      Xid id = mod.getIdentifiers().get(fctDef.getName().value());
      if(id == null) {
        throw new IllegalTransformationException(errorMsg, lineNo);
      }
      fctTypeMod = (XfunctionType) mod.getTypeTable().get(id.getType());
      if(fctTypeMod == null) {
        throw new IllegalTransformationException(errorMsg, lineNo);
      }
    }

    XbasicType modIntTypeIntentIn = mod.createBasicType(
        mod.getTypeTable().generateIntegerTypeHash(), Xname.TYPE_F_INT,
        Xintent.IN);
    mod.getTypeTable().add(modIntTypeIntentIn);

    List<Xnode> paramsLocal = fctType.getParams().getAll();
    List<Xnode> paramsMod = fctTypeMod.getParams().getAll();


    if(paramsLocal.size() < paramsMod.size()) {
      throw new IllegalTransformationException(
          "Local function has more parameters than module counterpart.",
          claw.getPragma().lineNo());
    }

    for(int i = 0; i < paramsLocal.size(); ++i) {
      Xnode pLocal = paramsLocal.get(i);
      // Number of parameters in the module function as been
      if(pLocal.getBooleanAttribute(ClawAttr.IS_CLAW.toString())) {
        // new parameter
        Xnode param = mod.createAndAddParamIfNotExists(pLocal.value(),
            modIntTypeIntentIn.getType(), fctTypeMod);
        if(param != null) {
          param.setAttribute(ClawAttr.IS_CLAW.toString(), Xname.TRUE);
        }
      } else {
        Xnode pMod = paramsMod.get(i);
        String localType = pLocal.getAttribute(Xattr.TYPE);
        String modType = pMod.getAttribute(Xattr.TYPE);

        if(!localType.equals(modType)) {
          // Param has been update so have to replicate the change to mod file
          XbasicType lType = (XbasicType) xcodeml.getTypeTable().get(localType);
          XbasicType crtType = (XbasicType) mod.getTypeTable().get(modType);

          List<ClawDimension> dimensions =
              TransformationHelper.findDimensions(fctType);
          OverPosition overPos = OverPosition.fromString(
              pLocal.getAttribute(ClawAttr.OVER.toString()));

          if(lType.isArray()) {
            String newType = TransformationHelper.duplicateWithDimension(lType,
                crtType, mod, xcodeml, overPos, dimensions);
            pMod.setAttribute(Xattr.TYPE, newType);
          }
        }

        // Propagate the over attribute
        copyAttribute(pLocal, pMod, ClawAttr.OVER);
      }
    }
  }

  /**
   * Copy the attribute from one Xnode to another Xnode if it exists in the
   * from node.
   *
   * @param from Xnode to copy from.
   * @param to   Xnode to copy to.
   * @param attr Attribute code to be copied.
   */
  private static void copyAttribute(Xnode from, Xnode to, ClawAttr attr) {
    if(from.hasAttribute(attr.toString())) {
      to.setAttribute(attr.toString(), from.getAttribute(attr.toString()));
    }
  }

  /**
   * Find the dimensions defined by the parallelize transformation.
   *
   * @param fctType Function type to analyze.
   * @return List of found dimensions.
   */
  public static List<ClawDimension> findDimensions(XfunctionType fctType) {
    List<ClawDimension> dimensions = new ArrayList<>();
    if(fctType.getParams() == null) {
      return dimensions;
    }
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.getBooleanAttribute(ClawAttr.IS_CLAW.toString())) {
        dimensions.add(
            new ClawDimension(
                ClawConstant.ITER_PREFIX + param.value(),
                ClawConstant.DEFAULT_LOWER_BOUND,
                param.value()
            )
        );
      }
    }
    return dimensions;
  }

  /**
   * Declare induction variables for dimensions if there are not present.
   *
   * @param dimensions List of dimensions.
   * @param fctDef     Function definition in which variable are created.
   * @param xcodeml    Current XcodeML program unit.
   */
  public static void declareInductionVariables(List<ClawDimension> dimensions,
                                               XfunctionDefinition fctDef,
                                               XcodeML xcodeml)
  {
    for(ClawDimension dim : dimensions) {
      if(fctDef.getDeclarationTable().get(dim.getIdentifier()) == null) {
        xcodeml.createIdAndDecl(dim.getIdentifier(), Xname.TYPE_F_INT,
            Xname.SCLASS_F_LOCAL, fctDef);
      }
    }
  }

  /**
   * Promote a field with the information stored in the defined dimensions.
   *
   * @param fieldId   Id of the field as defined in the symbol table.
   * @param update    If true, update current type otherwise, create a type from
   *                  scratch.
   * @param assumed   If true, generate assumed dimension range, otherwise, use
   *                  the information in the defined dimension.
   * @param overIndex Over clause to be used for promotion.
   * @param xcodeml   Current XcodeML program unit in which the element will be
   *                  created.
   * @param overPos   Force behavior as if over clause is present.
   * @throws IllegalTransformationException If type cannot be found.
   */
  public static PromotionInfo promoteField(String fieldId, boolean update,
                                           boolean assumed, int overIndex,
                                           int overDimensions,
                                           XfunctionDefinition fctDef,
                                           XfunctionType fctType,
                                           List<ClawDimension> dimensions,
                                           ClawLanguage claw,
                                           XcodeProgram xcodeml,
                                           OverPosition overPos)
      throws IllegalTransformationException
  {
    Xid id = fctDef.getSymbolTable().get(fieldId);
    Xdecl decl = fctDef.getDeclarationTable().get(fieldId);
    String type = xcodeml.getTypeTable().generateArrayTypeHash();
    XbasicType newType;

    if(update) {
      if(XnodeUtil.isBuiltInType(id.getType())) {
        newType = xcodeml.createBasicType(type, id.getType(), Xintent.NONE);
      } else {
        XbasicType old = (XbasicType) xcodeml.getTypeTable().get(id.getType());
        if(old == null) {
          throw new IllegalTransformationException("Cannot matchSeq type for " +
              fieldId, claw.getPragma().lineNo());
        } else {
          newType = old.cloneNode();
          newType.setType(type);
        }
      }
    } else {
      newType = xcodeml.createBasicType(type, id.getType(), Xintent.NONE);
    }

    PromotionInfo proInfo = new PromotionInfo(fieldId, newType.getDimensions(),
        newType.getDimensions() + dimensions.size(), type);

    if(assumed) {
      if(newType.isAllAssumedShape()
          && (fctType.hasParam(fieldId) || newType.isPointer()))
      {
        for(int i = 0; i < overDimensions; ++i) {
          Xnode index = xcodeml.createEmptyAssumedShaped();
          newType.addDimension(index, 0);
        }
      } else {
        if(claw.hasOverClause() || overPos != null) {
          /* If the directive has an over clause, there is three possibility to
           * insert the newly defined dimensions.
           * 1. Insert the dimensions in the middle on currently existing ones.
           * 2. Insert the dimensions before currently existing ones.
           * 3. Insert the dimensions after currently existing ones. */
          if(overPos == null) {
            overPos = getOverPosition(claw.getOverClauseValues().get(overIndex));
          }

          if(overPos == OverPosition.MIDDLE) {
            // Insert new dimension in middle (case 1)
            int startIdx = 1;
            for(ClawDimension dim : dimensions) {
              Xnode index = dim.generateIndexRange(xcodeml, false);
              newType.addDimension(index, startIdx++);
            }
          } else if(overPos == OverPosition.AFTER) {
            // Insert new dimensions at the end (case 3)
            for(ClawDimension dim : dimensions) {
              Xnode index = dim.generateIndexRange(xcodeml, false);
              newType.addDimension(index, XbasicType.APPEND);
            }
          } else {
            // Insert new dimension at the beginning (case 2)
            for(ClawDimension dim : dimensions) {
              Xnode index = dim.generateIndexRange(xcodeml, false);
              newType.addDimension(index, 0);
            }
          }
        } else {
          for(ClawDimension dim : dimensions) {
            Xnode index = dim.generateIndexRange(xcodeml, false);
            newType.addDimension(index, 0);
          }
        }
      }
    } else {
      for(ClawDimension dim : dimensions) {
        Xnode index = dim.generateIndexRange(xcodeml, false);
        newType.addDimension(index, XbasicType.APPEND);
      }
    }
    id.setType(type);
    decl.matchSeq(Xcode.NAME).setAttribute(Xattr.TYPE, type);
    xcodeml.getTypeTable().add(newType);


    // Update params in function type
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.value().equals(fieldId)) {

        // Update type with new promoted type
        param.setAttribute(Xattr.TYPE, type);

        // Save the over clause for parallelize forward transformation
        if(claw.hasOverClause()) {
          param.setAttribute(ClawAttr.OVER.toString(), getOverPosition(
              claw.getOverClauseValues().get(overIndex)).toString());
        }
      }
    }
    if(fctType.hasAttribute(Xattr.RESULT_NAME)
        && fctType.getAttribute(Xattr.RESULT_NAME).equals(fieldId))
    {
      if(claw.hasOverClause()) {
        fctType.setAttribute(ClawAttr.OVER.toString(), getOverPosition(
            claw.getOverClauseValues().get(overIndex)).toString());
      }
    }
    return proInfo;
  }

  /**
   * Get the enum value for the over position.
   *
   * @param overClause List of values in the over clause.
   * @return Position of the newly inserted dimensions compare the the existing
   * ones.
   */
  private static OverPosition getOverPosition(List<String> overClause) {
    if(overClause.get(0).equals(ClawDimension.BASE_DIM) &&
        overClause.get(overClause.size() - 1).equals(ClawDimension.BASE_DIM))
    {
      return OverPosition.MIDDLE;
    } else if(overClause.get(0).equals(ClawDimension.BASE_DIM)) {
      return OverPosition.AFTER;
    }
    return OverPosition.BEFORE;
  }

  /**
   * Get the number of base dimension in an over clause.
   *
   * @param over Over clause as a list of string element.
   * @return The number of base dimension.
   */
  public static int baseDimensionNb(List<String> over) {
    int cnt = 0;
    for(String dim : over) {
      if(dim.equals(ClawDimension.BASE_DIM)) {
        ++cnt;
      }
    }
    return cnt;
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   *
   * @param ids   List of array identifiers that must be adapted.
   * @param index Index designing the correct over clause to be used.
   */
  public static void adaptArrayReferences(List<String> ids, int index,
                                          Xnode parent,
                                          Map<String, PromotionInfo> promotions,
                                          List<List<Xnode>> beforeCrt,
                                          List<List<Xnode>> inMiddle,
                                          List<List<Xnode>> afterCrt,
                                          XcodeProgram xcodeml)
  {
    for(String data : ids) {
      if(promotions.get(data).wasScalar()) {
        List<Xnode> refs =
            XnodeUtil.getAllVarReferences(parent, data);
        for(Xnode ref : refs) {
          Xnode arrayRef = new Xnode(Xcode.FARRAYREF, xcodeml);
          Xnode varRef = new Xnode(Xcode.VARREF, xcodeml);
          arrayRef.setAttribute(Xattr.TYPE, ref.getAttribute(Xattr.TYPE));
          varRef.setAttribute(Xattr.TYPE, promotions.get(data).getTargetType());
          ref.setAttribute(Xattr.TYPE, promotions.get(data).getTargetType());
          ref.insertAfter(arrayRef);
          arrayRef.append(varRef, false);
          varRef.append(ref, false);
          for(Xnode ai : beforeCrt.get(index)) {
            arrayRef.append(ai, true);
          }
          for(Xnode ai : afterCrt.get(index)) {
            arrayRef.append(ai, true);
          }
        }
      } else {
        List<Xnode> refs =
            XnodeUtil.getAllArrayReferences(parent, data);
        for(Xnode ref : refs) {
          if(inMiddle.get(index).size() == 0) {
            for(Xnode ai : beforeCrt.get(index)) {
              ref.matchSeq(Xcode.VARREF).insertAfter(ai.cloneNode());
            }
            for(Xnode ai : afterCrt.get(index)) {
              ref.append(ai, true);
            }
          } else {
            Xnode hook =
                ref.matchDirectDescendant(Arrays.asList(Xcode.ARRAYINDEX, Xcode.INDEXRANGE));
            if(hook == null) {
              hook = ref.child(0);
            }
            for(Xnode ai : inMiddle.get(index)) {
              Xnode clone = ai.cloneNode();
              hook.insertAfter(clone);
              hook = clone;
            }
          }
        }
      }

    }
  }

  /**
   * Duplicates the type to update and add extra dimensions to match the base
   * type.
   *
   * @param base       Base type.
   * @param toUpdate   Type to update.
   * @param xcodemlDst Destination XcodeML unit. Duplicate will be created here.
   * @param xcodemlSrc Source XcodeML unit. Contains base dimension.
   * @return The new type hash generated.
   */
  public static String duplicateWithDimension(XbasicType base,
                                              XbasicType toUpdate,
                                              XcodeML xcodemlDst,
                                              XcodeML xcodemlSrc,
                                              OverPosition overPos,
                                              List<ClawDimension> dimensions)
      throws IllegalTransformationException
  {
    XbasicType newType = toUpdate.cloneNode();
    String type = xcodemlDst.getTypeTable().generateArrayTypeHash();
    newType.setAttribute(Xattr.TYPE, type);

    if(base.isAllAssumedShape() && toUpdate.isAllAssumedShape()) {
      int additionalDimensions =
          base.getDimensions() - toUpdate.getDimensions();
      for(int i = 0; i < additionalDimensions; ++i) {
        Xnode index = xcodemlDst.createEmptyAssumedShaped();
        newType.addDimension(index, 0);
      }
    } else if(base.isAllAssumedShape() && !toUpdate.isAllAssumedShape()) {
      switch(overPos) {
        case BEFORE:
          // TODO control and validate the before/after
          for(ClawDimension dim : dimensions) {
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false),
                XbasicType.APPEND);
          }
          break;
        case AFTER:
          for(ClawDimension dim : dimensions) {
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false), 0);
          }
          break;
        case MIDDLE:
          throw new IllegalTransformationException("Not supported yet. " +
              "Insertion in middle for duplicated array type.", 0);
      }
    } else {
      newType.resetDimension();

      for(int i = 0; i < base.getDimensions(); ++i) {
        Xnode newDim = new Xnode(Xcode.INDEXRANGE, xcodemlDst);
        newType.append(newDim, false);

        Xnode baseDim = base.getDimensions(i);
        Xnode lowerBound = baseDim.matchSeq(Xcode.LOWERBOUND);
        Xnode upperBound = baseDim.matchSeq(Xcode.UPPERBOUND);

        if(lowerBound != null) {
          Xnode newLowerBound =
              XnodeUtil.duplicateBound(lowerBound, xcodemlDst, xcodemlSrc);
          newDim.append(newLowerBound, false);
        }
        if(upperBound != null) {
          Xnode newUpperBound =
              XnodeUtil.duplicateBound(upperBound, xcodemlDst, xcodemlSrc);
          newDim.append(newUpperBound, false);
        }
        newType.addDimension(newDim, XbasicType.APPEND);
      }

    }

    xcodemlDst.getTypeTable().add(newType);
    return type;
  }
}
