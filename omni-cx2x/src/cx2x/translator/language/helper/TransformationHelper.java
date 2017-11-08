/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.language.helper;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.common.Utility;
import cx2x.translator.language.accelerator.AcceleratorDirective;
import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.base.Target;
import cx2x.translator.language.common.ClawReshapeInfo;
import cx2x.translator.transformation.claw.parallelize.PromotionInfo;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.language.InsertionPosition;
import cx2x.xcodeml.transformation.Translator;
import cx2x.xcodeml.xnode.*;
import exc.xcodeml.XcodeMLtools_Fmod;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

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
    XfunctionDefinition upperDef = fctDef.findParentFunction();
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
  private static Xnode getDeclInNestedFctDef(XfunctionDefinition fctDef,
                                             String name)
  {
    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getDeclarationTable().get(name);
    }
    XfunctionDefinition upperDef = fctDef.findParentFunction();
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
    XfunctionDefinition fctDef = claw.getPragma().findParentFunction();
    if(fctDef == null) {
      throw new IllegalTransformationException("Cannot apply reshape clause." +
          "Parent function definition not found.", claw.getPragma().lineNo());
    }

    for(ClawReshapeInfo reshapeInfo : claw.getReshapeClauseValues()) {
      Xid id = getIdInNestedFctDef(fctDef, reshapeInfo.getArrayName());
      Xnode decl = getDeclInNestedFctDef(fctDef, reshapeInfo.getArrayName());

      if(id == null || decl == null) {
        throw new IllegalTransformationException("Cannot apply reshape clause."
            + "Variable " + reshapeInfo.getArrayName() + " not found in " +
            "declaration table.", claw.getPragma().lineNo());
      }

      if(!(xcodeml.getTypeTable().isBasicType(id))) {
        throw new IllegalTransformationException(
            String.format("Reshape variable %s is not a basic type.",
                reshapeInfo.getArrayName()),
            claw.getPragma().lineNo()
        );
      }
      XbasicType crtType = xcodeml.getTypeTable().getBasicType(id);

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
      newType.setType(xcodeml.getTypeTable().generateHash(XcodeType.REAL));
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
      decl.matchSeq(Xcode.NAME).setType(newType.getType());

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
   * @param xcodeml    Current XcodeML file unit.
   * @param fctDef     Function definition that has been changed.
   * @param fctType    Function type that has been changed.
   * @param modDef     Module definition holding the function definition.
   * @param claw       Pragma that has triggered the transformation.
   * @param translator Current translator object.
   * @throws IllegalTransformationException If the module file or the function
   *                                        cannot be located
   */
  public static void updateModuleSignature(XcodeProgram xcodeml,
                                           XfunctionDefinition fctDef,
                                           XfunctionType fctType,
                                           XmoduleDefinition modDef,
                                           ClawLanguage claw,
                                           Translator translator,
                                           boolean importFctType)
      throws IllegalTransformationException
  {
    Xmod mod;
    if(translator.getModCache().isModuleLoaded(modDef.getName())) {
      mod = translator.getModCache().get(modDef.getName());
    } else {
      mod = fctDef.findContainingModule();
      if(mod == null) {
        throw new IllegalTransformationException(
            "Unable to locate module file for: " + modDef.getName(),
            claw.getPragma().lineNo());
      }
      translator.getModCache().add(modDef.getName(), mod);
    }

    XfunctionType fctTypeMod;
    if(importFctType) {
      // TODO should be part of XcodeML
      Node rawNode = mod.getDocument().importNode(fctType.element(), true);
      mod.getTypeTable().element().appendChild(rawNode);
      XfunctionType importedFctType = new XfunctionType((Element) rawNode);
      Xid importedFctTypeId = mod.createId(importedFctType.getType(),
          XstorageClass.F_FUNC, fctDef.getName().value());
      mod.getIdentifiers().add(importedFctTypeId);

      // check if params need to be imported as well
      if(importedFctType.getParameterNb() > 0) {
        for(Xnode param : importedFctType.getParams().getAll()) {
          mod.importType(xcodeml, param.getType());
        }
      }
      return;
    } else {
      fctTypeMod = mod.getTypeTable().getFunctionType(fctDef);
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
      fctTypeMod = mod.getTypeTable().getFunctionType(id);
      if(fctTypeMod == null) {
        throw new IllegalTransformationException(errorMsg, lineNo);
      }
    }

    XbasicType modIntTypeIntentIn =
        mod.createBasicType(XbuiltInType.INT, Xintent.IN);
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
      if(pLocal.getBooleanAttribute(Xattr.CLAW_PROMOTED)) {
        // new parameter
        Xnode param = mod.createAndAddParamIfNotExists(pLocal.value(),
            modIntTypeIntentIn.getType(), fctTypeMod);
        if(param != null) {
          param.setBooleanAttribute(Xattr.CLAW_PROMOTED, true);
        }
      } else {
        Xnode pMod = paramsMod.get(i);
        String localType = pLocal.getType();
        String modType = pMod.getType();

        if(!localType.equals(modType)) {
          // Param has been updated so have to replicate the change to mod file
          XbasicType lType = xcodeml.getTypeTable().getBasicType(pLocal);
          XbasicType crtType = mod.getTypeTable().getBasicType(pMod);

          InsertionPosition insPos = InsertionPosition.fromString(pLocal.getAttribute(Xattr.CLAW_OVER));

          List<DimensionDefinition> dimensions =
              TransformationHelper.findDimensions(fctType, insPos);

          if(lType.isArray()) {
            String newType = TransformationHelper.duplicateWithDimension(lType,
                crtType, mod, xcodeml, dimensions);
            pMod.setType(newType);
          }
        }

        // Propagate the over attribute
        copyAttribute(pLocal, pMod, Xattr.CLAW_OVER);
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
  private static void copyAttribute(Xnode from, Xnode to, Xattr attr) {
    if(from.hasAttribute(attr)) {
      to.setAttribute(attr, from.getAttribute(attr));
    }
  }

  /**
   * Find the dimensions defined by the parallelize transformation.
   *
   * @param fctType Function type to analyze.
   * @return List of found dimensions.
   */
  public static List<DimensionDefinition> findDimensions(XfunctionType fctType,
                                                         InsertionPosition pos)
  {
    List<DimensionDefinition> dimensions = new ArrayList<>();
    if(fctType.getParams() == null) {
      return dimensions;
    }
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.getBooleanAttribute(Xattr.CLAW_PROMOTED)) {
        DimensionDefinition dim = new DimensionDefinition(
            ClawConstant.ITER_PREFIX + param.value(),
            ClawConstant.DEFAULT_LOWER_BOUND,
            param.value()
        );
        dim.setInsertionPosition(pos);
        dimensions.add(dim);
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
  public static void declareInductionVariables(List<DimensionDefinition> dimensions,
                                               XfunctionDefinition fctDef,
                                               XcodeML xcodeml)
  {
    for(DimensionDefinition dim : dimensions) {
      if(fctDef.getDeclarationTable().get(dim.getIdentifier()) == null) {
        xcodeml.createIdAndDecl(dim.getIdentifier(), XbuiltInType.INT,
            XstorageClass.F_LOCAL, fctDef, false);
      }
    }
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
      if(dim.equals(DimensionDefinition.BASE_DIM)) {
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
          Xnode arrayRef = xcodeml.createNode(Xcode.FARRAYREF);
          Xnode varRef = xcodeml.createNode(Xcode.VARREF);
          arrayRef.setType(ref.getType());
          varRef.setType(promotions.get(data).getTargetType());
          ref.setType(promotions.get(data).getTargetType());
          ref.insertAfter(arrayRef);
          arrayRef.append(varRef);
          varRef.append(ref);
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
            Xnode hook = ref.matchDirectDescendant(
                Arrays.asList(Xcode.ARRAYINDEX, Xcode.INDEXRANGE));
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
                                              List<DimensionDefinition> dimensions)
      throws IllegalTransformationException
  {
    XbasicType newType = toUpdate.cloneNode();
    String type = xcodemlDst.getTypeTable().generateHash(XcodeType.ARRAY);
    newType.setType(type);

    if(base.isAllAssumedShape() && toUpdate.isAllAssumedShape()) {
      int additionalDimensions =
          base.getDimensions() - toUpdate.getDimensions();
      for(int i = 0; i < additionalDimensions; ++i) {
        Xnode index = xcodemlDst.createEmptyAssumedShaped();
        newType.addDimension(index, 0);
      }
    } else if(base.isAllAssumedShape() && !toUpdate.isAllAssumedShape()) {
      for(DimensionDefinition dim : dimensions) {
        switch(dim.getInsertionPosition()) {
          case BEFORE:
            // TODO control and validate the before/after
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false));
            break;
          case AFTER:
            newType.addDimension(dim.generateIndexRange(xcodemlDst, false), 0);
            break;
          case IN_MIDDLE:
            throw new IllegalTransformationException("Not supported yet. " +
                "Insertion in middle for duplicated array type.", 0);
        }
      }
    } else {
      newType.resetDimension();

      for(int i = 0; i < base.getDimensions(); ++i) {
        Xnode newDim = xcodemlDst.createNode(Xcode.INDEXRANGE);
        newType.append(newDim);

        Xnode baseDim = base.getDimensions(i);
        Xnode lowerBound = baseDim.matchSeq(Xcode.LOWERBOUND);
        Xnode upperBound = baseDim.matchSeq(Xcode.UPPERBOUND);

        if(lowerBound != null) {
          Xnode newLowerBound =
              XnodeUtil.duplicateBound(lowerBound, xcodemlDst, xcodemlSrc);
          newDim.append(newLowerBound);
        }
        if(upperBound != null) {
          Xnode newUpperBound =
              XnodeUtil.duplicateBound(upperBound, xcodemlDst, xcodemlSrc);
          newDim.append(newUpperBound);
        }
        newType.addDimension(newDim);
      }

    }

    xcodemlDst.getTypeTable().add(newType);
    return type;
  }
}
