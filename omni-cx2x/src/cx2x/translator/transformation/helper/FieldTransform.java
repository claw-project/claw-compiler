/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.helper;

import cx2x.translator.language.common.OverPosition;
import cx2x.translator.transformation.claw.parallelize.PromotionInfo;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.xnode.*;

/**
 * Low-level transformation applied fields. This included:
 * - Scalar and array promotion (promote)
 *
 * @author clementval
 */
public final class FieldTransform {

  // Avoid potential instantiation of this class
  private FieldTransform() {
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
      throw new IllegalTransformationException("Promotion information has not " +
          "enough information. Dimension empty!", decl.lineNo());
    }

    String type = xcodeml.getTypeTable().generateHash(XcodeType.ARRAY);
    XbasicType newType;
    if(crtType != null && crtType.isArray()) {
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
      Xintent newIntent = crtType != null ? crtType.getIntent() : Xintent.NONE;
      newType = xcodeml.createBasicType(type, id.getType(), newIntent);
    }

    fieldInfo.setBaseDimension(newType.getDimensions());
    fieldInfo.setTargetDimension(newType.getDimensions() +
        fieldInfo.getDimensions().size());
    fieldInfo.setTargetType(type);

    if(crtType != null && crtType.isArray()) {
      if(newType.isAllAssumedShape()
          && (fctType.hasParam(fieldInfo.getIdentifier())
          || newType.isAllocatable()
          || newType.isPointer()))
      {
        for(int i = 0; i < fieldInfo.diffDimension(); ++i) {
          Xnode index = xcodeml.createEmptyAssumedShaped();
          newType.addDimension(index, 0);
        }
      } else {
        if(fieldInfo.getOverPosition() != OverPosition.BEFORE) {
          /* If the directive has an over clause, there is three possibility to
           * insert the newly defined dimensions.
           * 1. Insert the dimensions in the middle on currently existing ones.
           * 2. Insert the dimensions before currently existing ones.
           * 3. Insert the dimensions after currently existing ones. */
         /* if(overPos == null) {
            overPos = OverPosition.fromList(
                claw.getOverClauseValues().get(overIndex));
          } */ // TODO set in PromotionInfo
          //fieldInfo.setOverPosition(overPos);

          if(fieldInfo.getOverPosition() == OverPosition.MIDDLE) {
            // Insert new dimension in middle (case 1)
            int startIdx = 1;
            for(DimensionDefinition dim : fieldInfo.getDimensions()) {
              Xnode index = dim.generateIndexRange(xcodeml, false);
              newType.addDimension(index, startIdx++);
            }
          } else if(fieldInfo.getOverPosition() == OverPosition.AFTER) {
            // Insert new dimensions at the end (case 3)
            for(DimensionDefinition dim : fieldInfo.getDimensions()) {
              Xnode index = dim.generateIndexRange(xcodeml, false);
              newType.addDimension(index);
            }
          }
        } else {
          // Case 2.
          for(DimensionDefinition dim : fieldInfo.getDimensions()) {
            Xnode index = dim.generateIndexRange(xcodeml, false);
            newType.addDimension(index, 0);
          }
        }
      }
    } else {
      for(DimensionDefinition dim : fieldInfo.getDimensions()) {
        Xnode index = dim.generateIndexRange(xcodeml, false);
        newType.addDimension(index);
      }
    }
    id.setType(type);
    decl.matchSeq(Xcode.NAME).setType(type);
    xcodeml.getTypeTable().add(newType);

    // Update params in function type
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.value().equals(fieldInfo.getIdentifier())) {

        // Update type with new promoted type
        param.setType(type);

        // Save the over clause for parallelize forward transformation
        if(fieldInfo.getOverPosition() != OverPosition.BEFORE) {
          param.setAttribute(Xattr.CLAW_OVER,
              fieldInfo.getOverPosition().toString());
        }
      }
    }
    if(fctType.hasAttribute(Xattr.RESULT_NAME)
        && fctType.getAttribute(Xattr.RESULT_NAME).
        equals(fieldInfo.getIdentifier()))
    {
      if(fieldInfo.getOverPosition() != OverPosition.BEFORE) {
        fctType.setAttribute(Xattr.CLAW_OVER,
            fieldInfo.getOverPosition().toString());
      }
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
          switch(promotionInfo.getOverPosition()) {
            case BEFORE:
              alloc.insert(dimension.generateAllocateNode(xcodeml));
              break;
            case MIDDLE:
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
}
