/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.InsertionPosition;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.xnode.common.Xattr;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

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
      if(argName.equalsIgnoreCase(arg.value())) {
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
  public static Xid findId(FfunctionDefinition fctDef, String name) {
    if(fctDef == null) {
      return null;
    }

    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getSymbolTable().get(name);
    }
    FfunctionDefinition upperDef = fctDef.findParentFunction();
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
  public static Xnode findDecl(FfunctionDefinition fctDef, String name) {
    if(fctDef.getSymbolTable().contains(name)) {
      return fctDef.getDeclarationTable().get(name);
    }
    FfunctionDefinition upperDef = fctDef.findParentFunction();
    if(upperDef == null) {
      return null;
    }
    return findDecl(upperDef, name);
  }

  /**
   * Read the promotion information stored in function type.
   *
   * @param fctType           Function type to read from.
   * @param insertionPosition Insertion position to be applied. Null to keep
   *                          original insertion position.
   * @return Promotion information object with read information.
   */
  public static PromotionInfo readPromotionInfo(FfunctionType fctType,
                                                InsertionPosition
                                                    insertionPosition)
  {
    PromotionInfo defaultInfo = new PromotionInfo();
    for(Xnode param : fctType.getParameters()) {
      if(param.hasAttribute(Xattr.PROMOTION_INFO)) {
        defaultInfo.
            readDimensionsFromString(param.getAttribute(Xattr.PROMOTION_INFO));
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

  /**
   * Gather all assignment statements in the function definition.
   * @param fctDef Function definition in which statements are gathered.
   * @return List of assignment statement in AST order. Empty list if function
   * definition is null or no statement found.
   */
  public static List<AssignStatement> gatherAssignStatements(
      FfunctionDefinition fctDef)
  {
    if(fctDef == null || fctDef.body() == null) {
      return Collections.emptyList();
    }
    List<AssignStatement> statements = new ArrayList<>();
    for(Xnode n : fctDef.body().matchAll(Xcode.F_ASSIGN_STATEMENT)) {
      statements.add(new AssignStatement(n.element()));
    }
    return statements;
  }
}
