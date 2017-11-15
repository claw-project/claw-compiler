/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package cx2x.translator.transformation.primitive;

import cx2x.translator.transformation.claw.one_column.PromotionInfo;
import cx2x.xcodeml.language.DimensionDefinition;
import cx2x.xcodeml.language.InsertionPosition;
import cx2x.xcodeml.xnode.*;

/**
 * Primitive transformation, test and utility for Function related action.
 * This includes:
 * - Find arguments in function call.
 * - Find id in function definition or in ancestor.
 *
 * @author clementval
 */
public final class Function {

  // Avoid instantiation of this class
  private Function() {
  }

  /**
   * Find specific argument in a function call.
   *
   * @param fctCall Function call node to search in.
   * @param argName Name of the argument to be found.
   * @return The argument if found. Null otherwise.
   */
  public static Xnode findArg(Xnode fctCall, String argName) {
    if(fctCall == null || fctCall.opcode() != Xcode.FUNCTION_CALL) {
      return null;
    }
    Xnode args = fctCall.matchSeq(Xcode.ARGUMENTS);
    if(args == null) {
      return null;
    }
    for(Xnode arg : args.children()) {
      if(argName.toLowerCase().equals(arg.value())) {
        return arg;
      }
    }
    return null;
  }

  /**
   * Find the id element in the current function definition or in parent
   * function definition if nested.
   *
   * @param fctDef Function definition.
   * @param name   Id name to be searched for.
   * @return The id if found. Null otherwise.
   */
  public static Xid findId(XfunctionDefinition fctDef, String name) {
    if(fctDef == null) {
      return null;
    }

    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getSymbolTable().get(name);
    }
    XfunctionDefinition upperDef = fctDef.findParentFunction();
    if(upperDef == null) {
      return null;
    }
    return findId(upperDef, name);
  }

  /**
   * Find the declaration element in the current function definition or in
   * parent if nested.
   *
   * @param fctDef Current function definition.
   * @param name   Declaration name to be searched for.
   * @return The element if found. Null otherwise.
   */
  public static Xnode findDecl(XfunctionDefinition fctDef, String name) {
    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getDeclarationTable().get(name);
    }
    XfunctionDefinition upperDef = fctDef.findParentFunction();
    if(upperDef == null) {
      return null;
    }
    return findDecl(upperDef, name);
  }

  /**
   * @param fctType
   * @param insertionPosition
   * @return
   */
  public static PromotionInfo readPromotionInfo(XfunctionType fctType,
                                                InsertionPosition
                                                    insertionPosition)
  {
    PromotionInfo defaultInfo = new PromotionInfo();
    for(Xnode param : fctType.getParams().getAll()) {
      if(param.hasAttribute(Xattr.PROMOTION_INFO)) {
        defaultInfo.readDimensionsFromString(param.getAttribute(Xattr.PROMOTION_INFO));
        break;
      }
    }
    if(insertionPosition != null && defaultInfo.getDimensions() != null) {
      for(DimensionDefinition dim : defaultInfo.getDimensions()) {
        dim.setInsertionPosition(insertionPosition);
      }
    }
    return defaultInfo;
  }
}
