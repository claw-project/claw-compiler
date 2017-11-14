/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.translator.common.ClawConstant;
import cx2x.translator.language.common.ClawReshapeInfo;
import cx2x.translator.transformation.claw.one_column.PromotionInfo;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.xnode.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * Primitive transformation and test applied to fields. This included:
 * - Scalar and array promotion (promote).
 * - Adaptation of scalar reference to array reference for promoted scalar.
 * - Adaptation of allocation statement for promoted field.
 *
 * @author clementval
 */
public final class Field {

  // Avoid potential instantiation of this class
  private Field() {
  }

  /**
   * Promote a field with the information stored in the defined dimensions.
   *
   * @param fieldInfo Promotion information. Must contains identifier and
   *                  dimensions.
   * @param fctDef    Function definition node in which the promotion is
   *                  performed.
   * @param xcodeml   Current XcodeML translation unit.
   * @throws IllegalTransformationException If promotion information are not
   *                                        sufficient. If types cannot be found
   *                                        in typeTable.
   */
  public static void promote(PromotionInfo fieldInfo,
                             XfunctionDefinition fctDef,
                             XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    Xid id = fctDef.getSymbolTable().get(fieldInfo.getIdentifier());
    Xnode decl = fctDef.getDeclarationTable().get(fieldInfo.getIdentifier());
    XbasicType crtType = xcodeml.getTypeTable().getBasicType(id);

    if(!XcodeType.isBuiltInType(id.getType()) && crtType == null) {
      throw new IllegalTransformationException("Basic type of field " +
          fieldInfo.getIdentifier() + " could not be found");
    }

    XfunctionType fctType = xcodeml.getTypeTable().getFunctionType(fctDef);
    if(fctType == null) {
      throw new IllegalTransformationException("Function type " +
          fctDef.getType() + " could not be found", fctDef.lineNo());
    }

    if(fieldInfo.getDimensions() == null
        || fieldInfo.getDimensions().size() == 0)
    {
      throw new IllegalTransformationException("Promotion information has not "
          + "enough information. Dimension empty!", decl.lineNo());
    }

    String type = xcodeml.getTypeTable().generateHash(XcodeType.ARRAY);
    XbasicType newType;
    if(crtType != null && crtType.isArray()) {
      fieldInfo.setPromotionType(PromotionInfo.PromotionType.ARRAY_TO_ARRAY);
      if(XcodeType.isBuiltInType(id.getType())) {
        newType = xcodeml.createBasicType(type, id.getType(), Xintent.NONE);
      } else {
        XbasicType old = xcodeml.getTypeTable().getBasicType(id);
        if(old == null) {
          throw new IllegalTransformationException("Cannot find type for " +
              fieldInfo.getIdentifier(), decl.lineNo());
        } else {
          newType = old.cloneNode();
          newType.setType(type);
        }
      }
    } else {
      fieldInfo.setPromotionType(PromotionInfo.PromotionType.SCALAR_TO_ARRAY);
      Xintent newIntent = crtType != null ? crtType.getIntent() : Xintent.NONE;
      newType = xcodeml.createBasicType(type, id.getType(), newIntent);
    }

    // Save promotion information (base dimensions, target dimensions, type)
    fieldInfo.setBaseDimension(newType.getDimensions());
    fieldInfo.setTargetDimension(newType.getDimensions() +
        fieldInfo.getDimensions().size());
    fieldInfo.setTargetType(type);

    if(fieldInfo.getPromotionType() ==
        PromotionInfo.PromotionType.ARRAY_TO_ARRAY)
    {
      if(newType.isAllAssumedShape()
          && (fctType.hasParam(fieldInfo.getIdentifier())
          || newType.isAllocatable() || newType.isPointer()))
      {
        for(int i = 0; i < fieldInfo.diffDimension(); ++i) {
          Xnode index = xcodeml.createEmptyAssumedShaped();
          newType.addDimension(index, 0);
        }
      } else {
        int beforePositionIndex = 0;
        int inMiddlePositionIndex = 1;

        for(DimensionDefinition dim : fieldInfo.getDimensions()) {
          switch(dim.getInsertionPosition()) {
            case BEFORE:
              newType.addDimension(dim.generateIndexRange(xcodeml, false),
                  beforePositionIndex++);
              inMiddlePositionIndex++; // Update index to insert in middle
              break;
            case IN_MIDDLE:
              newType.addDimension(dim.generateIndexRange(xcodeml, false),
                  inMiddlePositionIndex++);
              break;
            case AFTER:
              newType.addDimension(dim.generateIndexRange(xcodeml, false));
              break;
          }
        }
      }
    } else { // SCALAR to ARRAY promotion
      for(DimensionDefinition dim : fieldInfo.getDimensions()) {
        Xnode index = dim.generateIndexRange(xcodeml, false);
        newType.addDimension(index);
      }
    }

    // Set type hash to id and declaration node
    id.setType(type);
    decl.matchSeq(Xcode.NAME).setType(type);
    xcodeml.getTypeTable().add(newType);

    // Update params in function type with correct type hash
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.value().equals(fieldInfo.getIdentifier())) {

        // Update type with new promoted type
        param.setType(type);

        // Save the over clause for one_column forward transformation
        param.setAttribute(Xattr.CLAW_OVER,
            fieldInfo.getDimensions().get(0).getInsertionPosition().toString());
      }
    }

    if(fctType.hasAttribute(Xattr.RESULT_NAME)
        && fctType.getAttribute(Xattr.RESULT_NAME).
        equals(fieldInfo.getIdentifier()))
    {
      fctType.setAttribute(Xattr.CLAW_OVER,
          fieldInfo.getDimensions().get(0).getInsertionPosition().toString());
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   *
   * @param identifier List of array identifiers that must be adapted.
   * @param fctDef     Function definition in which reference are changed.
   * @param dims       Dimension definition to use for array index generation.
   * @param xcodeml    Current XcodeML program unit in which the element will be
   *                   created.
   */
  public static void adaptScalarRefToArrayRef(String identifier,
                                              XfunctionDefinition fctDef,
                                              List<DimensionDefinition> dims,
                                              XcodeML xcodeml)
  {
    List<Xnode> vars = XnodeUtil.findAllReferences(fctDef.body(), identifier);
    Xid sId = fctDef.getSymbolTable().get(identifier);
    XbasicType type = xcodeml.getTypeTable().getBasicType(sId);

    List<Xnode> arrayIndexes = new ArrayList<>();
    for(DimensionDefinition d : dims) {
      arrayIndexes.add(d.generateArrayIndex(xcodeml));
    }

    for(Xnode var : vars) {
      Xnode ref = xcodeml.createArrayRef(type, var.cloneNode());
      for(Xnode ai : arrayIndexes) {
        ref.append(ai, true);
      }
      var.insertAfter(ref);
      var.delete();
    }
  }

  /**
   * Adapt allocate statement with given dimension.
   *
   * @param promotionInfo Promotion information. Must contains identifier and
   *                      dimensions.
   * @param parent        Root node from which allocate statements are looked
   *                      for.
   * @param dimension     Dimension definition used to adapt the allocate
   *                      statement.
   * @param xcodeml       Current XcodeML translation unit.
   */
  public static void adaptAllocate(PromotionInfo promotionInfo, Xnode parent,
                                   DimensionDefinition dimension,
                                   XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    String arrayName = promotionInfo.getIdentifier();
    // Look through all allocate statements
    for(Xnode allocatedStmt : parent.matchAll(Xcode.FALLOCATESTATEMENT)) {
      for(Xnode alloc : allocatedStmt.matchAll(Xcode.ALLOC)) {
        Xnode var = alloc.matchDirectDescendant(Xcode.VAR);
        if(var != null && var.value().equals(arrayName)) {
          switch(dimension.getInsertionPosition()) {
            case BEFORE:
              alloc.insert(dimension.generateAllocateNode(xcodeml));
              break;
            case IN_MIDDLE:
              alloc.firstChild().
                  insertAfter(dimension.generateAllocateNode(xcodeml));
              break;
            case AFTER:
              alloc.append(dimension.generateAllocateNode(xcodeml));
              break;
          }
        }
      }
    }
  }

  /**
   * Demote an array reference to a var reference.
   *
   * @param arrayRef The array reference to be modified.
   */
  public static void demoteToScalar(Xnode arrayRef)
      throws IllegalTransformationException
  {
    if(arrayRef == null || arrayRef.opcode() != Xcode.FARRAYREF) {
      throw new IllegalTransformationException(ClawConstant.ERROR_INCOMPATIBLE);
    }

    Xnode var = arrayRef.matchSeq(Xcode.VARREF, Xcode.VAR).cloneNode();
    arrayRef.insertAfter(var);
    arrayRef.delete();
  }

  /**
   * Demote an array reference to a reference with fewer dimensions.
   *
   * @param arrayRef       The array reference to be modified.
   * @param keptDimensions List of dimensions to be kept. Dimension index starts
   *                       at 1.
   */
  public static void demote(Xnode arrayRef, List<Integer> keptDimensions)
      throws IllegalTransformationException
  {
    if(arrayRef == null || arrayRef.opcode() != Xcode.FARRAYREF) {
      throw new IllegalTransformationException(ClawConstant.ERROR_INCOMPATIBLE);
    }
    for(int i = 1; i < arrayRef.children().size(); ++i) {
      if(!keptDimensions.contains(i)) {
        arrayRef.child(i).delete();
      }
    }
  }

  /**
   * Reshape field in function definition.
   *
   * @param fctDef      Function definition.
   * @param reshapeInfo Reshape information.
   * @param xcodeml     Current XcodeML/F translation unit.
   * @throws IllegalTransformationException If reshape cannot be done.
   */
  public static void reshape(XfunctionDefinition fctDef,
                             ClawReshapeInfo reshapeInfo,
                             XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    Xid id = Function.findId(fctDef, reshapeInfo.getArrayName());
    Xnode decl = Function.findDecl(fctDef, reshapeInfo.getArrayName());

    if(id == null || decl == null) {
      throw new IllegalTransformationException("Cannot apply reshape clause."
          + "Variable " + reshapeInfo.getArrayName() + " not found in " +
          "declaration table.");
    }

    if(!(xcodeml.getTypeTable().isBasicType(id))) {
      throw new IllegalTransformationException(
          String.format("Reshape variable %s is not a basic type.",
              reshapeInfo.getArrayName())
      );
    }
    XbasicType crtType = xcodeml.getTypeTable().getBasicType(id);

    // Check dimension
    if(crtType.getDimensions() < reshapeInfo.getTargetDimension()) {
      throw new IllegalTransformationException(
          String.format(
              "Reshape variable %s has smaller dimension than requested.",
              reshapeInfo.getArrayName()
          )
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
        Field.demoteToScalar(ref);
      } else {
        Field.demote(ref, reshapeInfo.getKeptDimensions());
      }
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   *
   * @param id    Array identifier that must be adapted.
   * @param index Index designing the correct over clause to be used.
   */
  public static void adaptArrayReferences(String id, int index,
                                          Xnode parent,
                                          Map<String, PromotionInfo> promotions,
                                          List<List<Xnode>> beforeCrt,
                                          List<List<Xnode>> inMiddle,
                                          List<List<Xnode>> afterCrt,
                                          XcodeProgram xcodeml)
  {
    // TODO get rid of beforeCrt, inMiddle and afterCrt. Should be read from
    // promotion info

    if(!promotions.containsKey(id.toLowerCase())) {
      return;
    }

    if(promotions.get(id).wasScalar()) {
      List<Xnode> refs =
          XnodeUtil.getAllVarReferences(parent, id);
      for(Xnode ref : refs) {
        Xnode arrayRef = xcodeml.createNode(Xcode.FARRAYREF);
        Xnode varRef = xcodeml.createNode(Xcode.VARREF);
        arrayRef.setType(ref.getType());
        varRef.setType(promotions.get(id).getTargetType());
        ref.setType(promotions.get(id).getTargetType());
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
          XnodeUtil.getAllArrayReferences(parent, id);
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
    // TODO get rid of beforeCrt, inMiddle and afterCrt. Should be read from
    // promotion info
    for(String id : ids) {
      adaptArrayReferences(id, index, parent, promotions, beforeCrt,
          inMiddle, afterCrt, xcodeml);
    }
  }
}
