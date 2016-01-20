/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.xelement;

import java.util.Map;
import java.util.Hashtable;
import java.util.Collections;

/**
 * XelementName class contains all element and attributes values that can be
 * found in the XcodeML/F intermediate representation language.
 *
 * @author clementval
 */

public class XelementName {

  // helpers
  public static final String TRUE = "true";
  public static final String FALSE = "false";
  public static final String SUPPORTED_VERSION = "1.0";
  public static final String SUPPORTED_LANGUAGE = "Fortran";

  // Base type (9.1)
  public static final String TYPE_F_INT = "Fint";
  public static final String TYPE_F_REAL = "Freal";
  public static final String TYPE_F_COMPLEX = "Fcomplex";
  public static final String TYPE_F_LOGICAL = "Flogical";
  public static final String TYPE_F_CHAR = "Fcharacter";
  public static final String TYPE_F_VOID = "Fvoid";

  // Scope
  public static final String SCOPE_LOCAL = "local";
  public static final String SCOPE_GLOBAL = "global";
  public static final String SCOPE_PARAM = "param";

  // Element attributes
  public static final String ATTR_COMPILER_INFO = "compiler-info";
  public static final String ATTR_CONSTRUCT_NAME = "construct_name";
  public static final String ATTR_FILE = "file";
  public static final String ATTR_INTENT = "intent";
  public static final String ATTR_IS_ASSUMED_SHAPE = "is_assumed_shape";
  public static final String ATTR_IS_ALLOCATABLE = "is_allocatable";
  public static final String ATTR_IS_EXTERNAL = "is_external";
  public static final String ATTR_IS_INTERNAL = "is_internal";
  public static final String ATTR_IS_INTRINSIC = "is_intrinsic";
  public static final String ATTR_IS_OPTIONAL = "is_optional";
  public static final String ATTR_IS_PARAMETER = "is_parameter";
  public static final String ATTR_IS_POINTER = "is_pointer";
  public static final String ATTR_IS_PRIVATE = "is_private";
  public static final String ATTR_IS_PROGRAM = "is_program";
  public static final String ATTR_IS_PUBLIC = "is_public";
  public static final String ATTR_IS_RECURSIVE = "is_recursive";
  public static final String ATTR_IS_SAVE = "is_save";
  public static final String ATTR_IS_TARGET = "is_target";
  public static final String ATTR_KIND = "kind";
  public static final String ATTR_LANGUAGE = "language";
  public static final String ATTR_LINENO = "lineno";
  public static final String ATTR_REF = "ref";
  public static final String ATTR_RESULT_NAME = "result_name";
  public static final String ATTR_RETURN_TYPE = "return_type";
  public static final String ATTR_SCLASS = "sclass";
  public static final String ATTR_SCOPE = "scope";
  public static final String ATTR_SOURCE = "source";
  public static final String ATTR_TIME = "time";
  public static final String ATTR_TYPE = "type";
  public static final String ATTR_VERSION = "version";

  // Element names
  public static final String ARGUMENTS = "arguments";
  public static final String ARRAY_INDEX = "arrayIndex";
  public static final String BASIC_TYPE = "FbasicType";
  public static final String BODY = "body";
  public static final String CONDITION = "condition";
  public static final String DECLARATIONS = "declarations";
  public static final String DO_STMT = "FdoStatement";
  public static final String ELSE = "else";
  public static final String EXPR_STMT = "exprStatement";
  public static final String FCT_CALL = "functionCall";
  public static final String FCT_DEFINITION = "FfunctionDefinition";
  public static final String FCT_TYPE = "FfunctionType";
  public static final String F_ARRAY_REF = "FarrayRef";
  public static final String F_CHAR_REF = "FcharacterRef";
  public static final String F_COARRAY_REF = "FcoArrayRef";
  public static final String F_IF_STMT = "FifStatement";
  public static final String F_INT_CONST = "FintConstant";
  public static final String F_REAL_CONST = "FrealConstant";
  public static final String F_COMPLEX_CONST = "FcomplexConstant";
  public static final String F_CHAR_CONST = "FcharacterConstant";
  public static final String F_LOGICAL_CONST = "FlogicalConstant";
  public static final String F_MEMBER_REF = "FmemberRef";
  public static final String F_STRUCT_TYPE = "FstructType";
  public static final String GLOBAL_DECLARATIONS = "globalDeclarations";
  public static final String GLOBAL_SYMBOLS = "globalSymbols";
  public static final String ID = "id";
  public static final String INDEX_RANGE = "indexRange";
  public static final String KIND = "kind";
  public static final String LENGTH = "len";
  public static final String LOWER_BOUND = "lowerBound";
  public static final String NAME = "name";
  public static final String PRAGMA_STMT = "FpragmaStatement";
  public static final String STEP = "step";
  public static final String SYMBOLS = "symbols";
  public static final String THEN = "then";
  public static final String TYPE_TABLE = "typeTable";
  public static final String UPPER_BOUND = "upperBound";
  public static final String VAR = "Var";
  public static final String VAR_DECL = "varDecl";
  public static final String VAR_REF = "varRef";
  public static final String VALUE = "value";
  public static final String X_CODE_PROGRAM = "XcodeProgram";


  private static Map<Class, String> _classToElementNameMapping;

  static {
    Map<Class, String> tempMap = new Hashtable<>();
    tempMap.put(XargumentsTable.class, ARGUMENTS);
    tempMap.put(XarrayIndex.class, ARRAY_INDEX);
    tempMap.put(XbasicType.class, BASIC_TYPE);
    tempMap.put(Xbody.class, BODY);
    tempMap.put(Xcondition.class, CONDITION);
    tempMap.put(XdeclTable.class, DECLARATIONS);
    tempMap.put(XdoStatement.class, DO_STMT);
    tempMap.put(Xelse.class, ELSE);
    tempMap.put(XexprStatement.class, EXPR_STMT);
    tempMap.put(XfctCall.class, FCT_CALL);
    tempMap.put(XfctDef.class, FCT_DEFINITION);
    tempMap.put(XfctType.class, FCT_TYPE);
    tempMap.put(XarrayRef.class, F_ARRAY_REF);
    tempMap.put(XifStatement.class, F_IF_STMT);
    tempMap.put(XintConstant.class, F_INT_CONST);
    tempMap.put(XrealConstant.class, F_REAL_CONST);
    tempMap.put(XcomplexConstant.class, F_COMPLEX_CONST);
    tempMap.put(XcharacterConstant.class, F_CHAR_CONST);
    tempMap.put(XlogicalConstant.class, F_LOGICAL_CONST);
    //tempMap.put(XstructType.class, F_STRUCT_TYPE); TODO Uncomment when XstructType available
    tempMap.put(XglobalDeclTable.class, GLOBAL_DECLARATIONS);
    tempMap.put(XglobalSymbolTable.class, GLOBAL_SYMBOLS);
    tempMap.put(Xid.class, ID);
    tempMap.put(XindexRange.class, INDEX_RANGE);
    tempMap.put(Xkind.class, KIND);
    tempMap.put(Xlength.class, LENGTH);
    tempMap.put(XlowerBound.class, LOWER_BOUND);
    tempMap.put(Xname.class, NAME);
    tempMap.put(Xpragma.class, PRAGMA_STMT);
    tempMap.put(Xstep.class, STEP);
    tempMap.put(XsymbolTable.class, SYMBOLS);
    tempMap.put(Xthen.class, THEN);
    tempMap.put(XtypeTable.class, TYPE_TABLE);
    tempMap.put(XupperBound.class, UPPER_BOUND);
    tempMap.put(Xvar.class, VAR);
    tempMap.put(XvarDecl.class, VAR_DECL);
    tempMap.put(XvarRef.class, VAR_REF);
    tempMap.put(Xvalue.class, VALUE);
    tempMap.put(XcodeProg.class, X_CODE_PROGRAM);
    _classToElementNameMapping = Collections.unmodifiableMap(tempMap);
  }

  /**
   * Get the corresponding element name from a XbaseElement derived class
   * @param xElementClass XbaseElement derived class
   * @return String value of the element name if know. Null otherwise
   */
  public static <T extends XbaseElement> String
    getElementNameFromClass(Class<T> xElementClass)
  {
    if(_classToElementNameMapping.containsKey(xElementClass)){
      return _classToElementNameMapping.get(xElementClass);
    }
    return null;
  }

}
