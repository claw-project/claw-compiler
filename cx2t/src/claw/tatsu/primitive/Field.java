/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.TatsuConstant;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.abstraction.ReshapeInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.*;

import java.util.ArrayList;
import java.util.List;

/**
 * Primitive transformation and test applied to fields. This included:
 * - Scalar and array promotion (promote).
 * - Adaptation of scalar reference to array reference for promoted scalar.
 * - Adaptation of allocate statement for promoted field.
 * - Demote array reference to fewer dimension or scalar.
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
                             FfunctionDefinition fctDef,
                             XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    Xid id = fctDef.getSymbolTable().get(fieldInfo.getIdentifier());
    Xnode decl = fctDef.getDeclarationTable().get(fieldInfo.getIdentifier());
    FbasicType crtType = xcodeml.getTypeTable().getBasicType(id);

    if(!FortranType.isBuiltInType(id.getType()) && crtType == null) {
      throw new IllegalTransformationException("Basic type of field " +
          fieldInfo.getIdentifier() + " could not be found");
    }

    FfunctionType fctType = xcodeml.getTypeTable().getFunctionType(fctDef);
    if(fctType == null) {
      throw new IllegalTransformationException("Function type " +
          fctDef.getType() + " could not be found", fctDef.lineNo());
    }

    if(fieldInfo.getDimensions() == null
        || fieldInfo.getDimensions().isEmpty())
    {
      throw new IllegalTransformationException("Promotion information has not "
          + "enough information. Dimension empty!", decl.lineNo());
    }

    String type = xcodeml.getTypeTable().generateHash(FortranType.ARRAY);
    FbasicType newType;
    if(crtType != null && crtType.isArray()) {
      fieldInfo.setPromotionType(PromotionInfo.PromotionType.ARRAY_TO_ARRAY);
      if(FortranType.isBuiltInType(id.getType())) {
        newType = xcodeml.createBasicType(type, id.getType(), Intent.NONE);
      } else {
        FbasicType old = xcodeml.getTypeTable().getBasicType(id);
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
      Intent newIntent = crtType != null ? crtType.getIntent() : Intent.NONE;
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
                  beforePositionIndex);
              ++beforePositionIndex;
              ++inMiddlePositionIndex; // Update index to insert in middle
              break;
            case IN_MIDDLE:
              newType.addDimension(dim.generateIndexRange(xcodeml, false),
                  inMiddlePositionIndex);
              ++inMiddlePositionIndex;
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
    for(Xnode param : fctType.getParameters()) {
      if(param.value().equals(fieldInfo.getIdentifier())) {

        // Update type with new promoted type
        param.setType(type);

        // Save the over clause for one_column forward transformation
        param.setAttribute(Xattr.PROMOTION_INFO,
            fieldInfo.getFormattedDimensions());
      }
    }

    if(fctType.hasAttribute(Xattr.RESULT_NAME)
        && fctType.getAttribute(Xattr.RESULT_NAME).
        equals(fieldInfo.getIdentifier()))
    {
      fctType.setAttribute(Xattr.PROMOTION_INFO,
          fieldInfo.getFormattedDimensions());
    }
  }

  /**
   * Adapt all the array references of the variable in the data clause in the
   * current function/subroutine definition.
   *
   * @param promotionInfo Object holding the promotion information.
   * @param fctDef        Function definition in which reference are changed.
   * @param dims          Dimension definition to use for array index
   *                      generation.
   * @param xcodeml       Current XcodeML program unit in which the element
   *                      will be created.
   */
  public static void adaptScalarRefToArrayRef(PromotionInfo promotionInfo,
                                              FfunctionDefinition fctDef,
                                              List<DimensionDefinition> dims,
                                              XcodeML xcodeml)
  {
    if(promotionInfo.isRefAdapted()) {
      return;
    }

    List<Xnode> vars = XnodeUtil.findAllReferences(fctDef.body(),
        promotionInfo.getIdentifier());
    Xid sId = fctDef.getSymbolTable().get(promotionInfo.getIdentifier());
    FbasicType type = xcodeml.getTypeTable().getBasicType(sId);

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
    promotionInfo.setRefAdapted();
  }

  /**
   * Adapt allocate statement with given dimension.
   *
   * @param promotionInfo Promotion information. Must contains identifier and
   *                      dimensions.
   * @param parent        Root node from which allocate statements are looked
   *                      for.
   * @param xcodeml       Current XcodeML translation unit.
   */
  public static void adaptAllocate(PromotionInfo promotionInfo, Xnode parent,
                                   XcodeProgram xcodeml)
  {
    if(promotionInfo.isAllocateAdapted()) {
      return;
    }

    String arrayName = promotionInfo.getIdentifier();
    // Look through all allocate statements
    for(Xnode allocatedStmt : parent.matchAll(Xcode.F_ALLOCATE_STATEMENT)) {
      for(Xnode alloc : allocatedStmt.matchAll(Xcode.ALLOC)) {
        Xnode var = alloc.matchDirectDescendant(Xcode.VAR);
        if(var != null && var.value().equals(arrayName)) {
          int beforePositionIndex = 0; // First arrayIndex after varRef at pos 0
          int inMiddlePositionIndex = 1;
          for(DimensionDefinition dim : promotionInfo.getDimensions()) {
            switch(dim.getInsertionPosition()) {
              case BEFORE:
                if(beforePositionIndex == 0) {
                  alloc.insert(dim.generateAllocateNode(xcodeml));
                } else {
                  alloc.child(beforePositionIndex).
                      insertBefore(dim.generateArrayIndex(xcodeml));
                }
                ++beforePositionIndex;
                ++inMiddlePositionIndex;
                break;
              case IN_MIDDLE:
                alloc.child(inMiddlePositionIndex).
                    insertAfter(dim.generateArrayIndex(xcodeml));
                ++inMiddlePositionIndex;
                break;
              case AFTER:
                alloc.append(dim.generateArrayIndex(xcodeml));
                break;
            }
          }
        }
      }
    }
    promotionInfo.setAllocateAdapted();
  }

  /**
   * Demote an array reference to a var reference.
   *
   * @param arrayRef The array reference to be modified.
   * @throws IllegalTransformationException If node is null or not an FarrayRef.
   */
  private static void demoteToScalar(Xnode arrayRef)
      throws IllegalTransformationException
  {
    if(arrayRef == null || arrayRef.opcode() != Xcode.F_ARRAY_REF) {
      throw new
          IllegalTransformationException(TatsuConstant.ERROR_INCOMPATIBLE);
    }

    Xnode var = arrayRef.matchSeq(Xcode.VAR_REF, Xcode.VAR).cloneNode();
    arrayRef.insertAfter(var);
    arrayRef.delete();
  }

  /**
   * Demote an array reference to a reference with fewer dimensions.
   *
   * @param arrayRef       The array reference to be modified.
   * @param keptDimensions List of dimensions to be kept. Dimension index starts
   *                       at 1.
   * @throws IllegalTransformationException If node is null or not an FarrayRef.
   */
  private static void demote(Xnode arrayRef, List<Integer> keptDimensions)
      throws IllegalTransformationException
  {
    if(arrayRef == null || arrayRef.opcode() != Xcode.F_ARRAY_REF) {
      throw new
          IllegalTransformationException(TatsuConstant.ERROR_INCOMPATIBLE);
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
  public static void reshape(FfunctionDefinition fctDef,
                             ReshapeInfo reshapeInfo,
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
    FbasicType crtType = xcodeml.getTypeTable().getBasicType(id);

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
    FbasicType newType = crtType.cloneNode();
    newType.setType(xcodeml.getTypeTable().generateHash(FortranType.REAL));
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
   * @param promotionInfo Promotion information used for the promotion of the
   *                      field.
   * @param parent        Root node of the tree in which the adaptation is done.
   * @param xcodeml       Current XcodeML translation unit.
   */
  public static void adaptArrayRef(PromotionInfo promotionInfo, Xnode parent,
                                   XcodeProgram xcodeml)
  {
    if(promotionInfo.isRefAdapted()) {
      return;
    }

    // Scalar to array reference
    if(promotionInfo.wasScalar()) {
      List<Xnode> refs =
          XnodeUtil.getAllVarReferences(parent, promotionInfo.getIdentifier());
      for(Xnode ref : refs) {

        if(Function.isArgOfFunction(ref, Xintrinsic.PRESENT)) {
          continue;
        }

        Xnode arrayRef = xcodeml.createNode(Xcode.F_ARRAY_REF);
        Xnode varRef = xcodeml.createNode(Xcode.VAR_REF);
        arrayRef.setType(ref.getType());
        varRef.setType(promotionInfo.getTargetType());
        ref.setType(promotionInfo.getTargetType());
        ref.insertAfter(arrayRef);
        arrayRef.append(varRef);
        varRef.append(ref);

        // Simply generate all arrayIndex in order
        for(DimensionDefinition dim : promotionInfo.getDimensions()) {
          arrayRef.append(dim.generateArrayIndex(xcodeml));
        }
      }
    } else { // Array reference to array reference
      List<Xnode> refs = XnodeUtil.getAllArrayReferences(parent,
          promotionInfo.getIdentifier());
      for(Xnode ref : refs) {
        if(ref.matchAncestor(Xcode.F_ALLOCATE_STATEMENT) != null) {
          continue;
        }
        int beforePositionIndex = 0; // First arrayIndex after varRef at pos 0
        int inMiddlePositionIndex = 1;
        for(DimensionDefinition dim : promotionInfo.getDimensions()) {
          switch(dim.getInsertionPosition()) {
            case BEFORE:
              ref.child(beforePositionIndex).
                  insertAfter(dim.generateArrayIndex(xcodeml));
              ++beforePositionIndex;
              ++inMiddlePositionIndex;
              break;
            case IN_MIDDLE:
              ref.child(inMiddlePositionIndex).
                  insertAfter(dim.generateArrayIndex(xcodeml));
              ++inMiddlePositionIndex;
              break;
            case AFTER:
              ref.append(dim.generateArrayIndex(xcodeml));
              break;
          }
        }
      }
    }
    promotionInfo.setRefAdapted();
  }
}
