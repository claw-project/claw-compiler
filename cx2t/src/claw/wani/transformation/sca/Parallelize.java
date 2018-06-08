/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.*;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.*;
import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.abstraction.DimensionDefinition;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.*;
import claw.tatsu.xcodeml.xnode.fortran.*;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawTransformation;
import claw.wani.x2t.configuration.Configuration;
import claw.wani.x2t.configuration.openacc.OpenAccLocalStrategy;

import java.util.*;

/**
 * The Single Column Abstraction (SCA) transformation transforms the code
 * contained in a subroutine/function by adding necessary dimensions and
 * parallelism to the defined data.
 *
 * Transformation for the GPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Do statements over the additional dimensions is added as an outer
 * loop and wrap the entire body of the subroutine.
 * </ul>
 *
 * Transformation for the CPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Propagated promotion is applied to all scalars or arrays used in an
 * assign statement at the lhs and where a promoted variable is used on the
 * rhs.
 * <li> Do statements over the additional dimensions are added as an inner
 * loop wrapping each assign statements including promoted variables.
 * </ul>
 *
 * Generation of OpenACC directives:<ul>
 * <li> acc routine seq is generated for subroutine called from the parallelized
 * subroutine if they are located in the same translation unit.
 * <li> acc data region with corresponding present clause for all promoted
 * variables with the intent in, out or inout.
 * <li> acc parallel region is generated to wrap all the body of the subroutine.
 * <li> acc private clause is added to the parallel directive for all local
 * variables.
 * <li> acc loop is generated for the generated do statement.
 * <li> acc loop seq is generated for already existing do statements.
 * </ul>
 *
 * Generation of OpenMP directives: <ul>
 * <li> omp parallel do is generated for each generated do statements.
 * </ul>
 *
 * @author clementval
 */
public class Parallelize extends ClawTransformation {

  private final Map<String, DimensionDefinition> _dimensions;
  private final Map<String, PromotionInfo> _promotions;
  private final Set<String> _arrayFieldsInOut;
  private final Set<String> _scalarFields;
  private int _overDimensions;
  private FfunctionDefinition _fctDef;
  private FfunctionType _fctType;

  /**
   * Constructs a new Parallelize transformation triggered from a specific
   * pragma.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public Parallelize(ClawPragma directive) {
    super(directive);
    _overDimensions = 0;
    _dimensions = new HashMap<>();
    _promotions = new HashMap<>();
    _arrayFieldsInOut = new HashSet<>();
    _scalarFields = new HashSet<>();
  }

  /**
   * Print information about promoted arrays, candidate for promotion arrays and
   * scalars.
   *
   * @param name            Name of the subroutine.
   * @param promoted        List of promoted array variables.
   * @param candidateArrays List of candidate array variables for promotion.
   * @param scalars         List of candidate scalar variables for promotion.
   */
  private void printDebugPromotionInfos(String name, Set<String> promoted,
                                        List<String> candidateArrays,
                                        List<String> scalars)
  {
    Message.debug("==========================================");
    Message.debug("Parallelize promotion infos for subroutine " + name);
    Message.debug("  - Promoted arrays(" + promoted.size() + "):");
    for(String array : promoted) {
      Message.debug("      " + array);
    }
    Message.debug("  - Candidate arrays(" + candidateArrays.size() + "):");
    for(String array : candidateArrays) {
      Message.debug("      " + array);
    }
    Message.debug("  - Candidate scalars(" + scalars.size() + "):");
    for(String array : scalars) {
      Message.debug("      " + array);
    }
    Message.debug("==========================================");
  }

  @Override
  public boolean analyze(XcodeProgram xcodeml, Translator translator) {

    // Check for the parent fct/subroutine definition
    _fctDef = _claw.getPragma().findParentFunction();
    if(_fctDef == null) {
      xcodeml.addError("Parent function/subroutine cannot be found. " +
              "Parallelize directive must be defined in a function/subroutine.",
          _claw.getPragma().lineNo());
      return false;
    }
    _fctType = xcodeml.getTypeTable().getFunctionType(_fctDef);
    if(_fctType == null) {
      xcodeml.addError("Function/subroutine signature cannot be found. ",
          _claw.getPragma().lineNo());
      return false;
    }

    /* Check if unsupported statements are located in the future parallel
     * region. */
    if(Context.get().getTarget() == Target.GPU
        && Context.get().getGenerator().getDirectiveLanguage()
        == CompilerDirective.OPENACC)
    {
      Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
      Xnode parallelRegionStart =
          Directive.findParallelRegionStart(_fctDef, null);
      Xnode parallelRegionEnd =
          Directive.findParallelRegionEnd(_fctDef, contains);

      List<Xnode> unsupportedStatements =
          XnodeUtil.getNodes(parallelRegionStart, parallelRegionEnd,
              Context.get().getGenerator().getUnsupportedStatements());

      if(unsupportedStatements.size() > 0) {
        for(Xnode statement : unsupportedStatements) {
          xcodeml.addError("Unsupported statement in parallel region",
              statement.lineNo());
        }
        return false;
      }
    }

    return analyzeDimension(xcodeml) && analyzeData(xcodeml) &&
        analyzeOver(xcodeml);
  }

  /**
   * Analyse the defined dimension.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeDimension(XcodeProgram xcodeml) {
    if(!_claw.hasDimensionClause()) {
      xcodeml.addError("No dimension defined for parallelization.",
          _claw.getPragma().lineNo());
      return false;
    }

    for(DimensionDefinition d : _claw.getDimensionValues()) {
      if(_dimensions.containsKey(d.getIdentifier())) {
        xcodeml.addError(
            String.format("Dimension with identifier %s already specified.",
                d.getIdentifier()), _claw.getPragma().lineNo()
        );
        return false;
      }
      _dimensions.put(d.getIdentifier(), d);
    }
    return true;
  }

  /**
   * Analyse the information defined in the data clause.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeData(XcodeProgram xcodeml) {
    /* If there is no data/over clause specified, an automatic deduction for
     * array promotion is performed. */
    if(!_claw.hasOverDataClause()) {

      List<String> scalars = new ArrayList<>();
      List<String> candidateArrays = new ArrayList<>();

      List<Xnode> declarations = _fctDef.getDeclarationTable().values();
      for(Xnode decl : declarations) {
        if(decl.opcode() != Xcode.VAR_DECL) {
          continue;
        }

        if(xcodeml.getTypeTable().isBasicType(decl)) {
          String varName = decl.matchSeq(Xcode.NAME).value();
          FbasicType bType = xcodeml.getTypeTable().getBasicType(decl);

          if(bType.isArray()) {
            if(bType.hasIntent() || bType.isPointer()) {
              _arrayFieldsInOut.add(varName);
            } else {
              candidateArrays.add(varName);
            }
          } else {
            if(_claw.hasScalarClause() &&
                _claw.getScalarClauseValues().contains(varName))
            {
              _arrayFieldsInOut.add(varName);
            }
            if(!bType.isParameter() && bType.hasIntent()) {
              scalars.add(varName);
            }
          }
        }
      }
      _scalarFields.addAll(scalars);
      _scalarFields.addAll(candidateArrays);

      printDebugPromotionInfos(_fctDef.getName(), _arrayFieldsInOut,
          candidateArrays, scalars);

      return true;
    }

    /* If the data clause if defined at least once, manual promotion is the
     * rule. The array identifiers defined in the data clauses will be used as
     * the list of array to be promoted.
     * In the analysis, we control that all defined arrays in the data clauses
     * are actual declared variables. */
    for(List<String> data : _claw.getOverDataClauseValues()) {
      for(String d : data) {
        if(!_fctDef.getSymbolTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not defined in the current block.", d),
              _claw.getPragma().lineNo()
          );
          return false;
        }
        if(!_fctDef.getDeclarationTable().contains(d)) {
          xcodeml.addError(
              String.format("Data %s is not declared in the current block.", d),
              _claw.getPragma().lineNo()
          );
          return false;
        }
      }
      _arrayFieldsInOut.addAll(data);
    }
    return true;
  }

  /**
   * Analyse the information defined in the over clause.
   *
   * @param xcodeml Current XcodeML program unit to store the error message.
   * @return True if the analysis succeeded. False otherwise.
   */
  private boolean analyzeOver(XcodeProgram xcodeml) {
    if(!_claw.hasOverClause()) {
      _overDimensions += _claw.getDimensionValues().size();
      return true;
    }

    // Check if over dimensions are defined dimensions
    _overDimensions = _claw.getDimensionValues().size();
    for(List<String> overLst : _claw.getOverClauseValues()) {
      int usedDimension = 0;
      for(String o : overLst) {
        if(!o.equals(DimensionDefinition.BASE_DIM)) {
          if(!_dimensions.containsKey(o)) {
            xcodeml.addError(
                String.format(
                    "Dimension %s is not defined. Cannot be used in over " +
                        "clause", o), _claw.getPragma().lineNo()
            );
            return false;
          }
          ++usedDimension;
        }
      }
      if(usedDimension != _overDimensions) {
        xcodeml.addError("Over clause doesn't use one or more defined " +
            "dimensions", _claw.getPragma().lineNo());
        return false;
      }
    }

    return true;
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Handle PURE function / subroutine
    boolean pureRemoved = _fctType.isPure();
    _fctType.removeAttribute(Xattr.IS_PURE);
    if(Configuration.get().isForcePure() && pureRemoved) {
      throw new IllegalTransformationException(
          "PURE specifier cannot be removed", _fctDef.lineNo());
    } else if(pureRemoved) {
      String fctName = _fctDef.matchDirectDescendant(Xcode.NAME).value();
      xcodeml.addWarning("PURE specifier removed from function " + fctName +
              ". Transformation and code generation applied to it.",
          _fctDef.lineNo());
    }

    // Insert the declarations of variables to iterate over the new dimensions.
    insertVariableToIterateOverDimension(xcodeml);

    // Promote all array fields with new dimensions.
    promoteFields(xcodeml);

    // Adapt array references.
    if(_claw.hasOverDataClause()) {
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i) {
        for(String id : _claw.getOverDataClauseValues().get(i)) {
          Field.adaptArrayRef(_promotions.get(id), _fctDef.body(), xcodeml);
        }
      }
    } else {
      for(String id : _arrayFieldsInOut) {
        Field.adaptArrayRef(_promotions.get(id), _fctDef.body(), xcodeml);
      }
    }

    removePragma();

    // Apply specific target transformation
    if(Context.get().getTarget() == Target.GPU) {
      transformForGPU(xcodeml);
    } else if(Context.get().getTarget() == Target.CPU
        || Context.get().getTarget() == Target.ARM)
    {
      transformPartialForCPU(xcodeml);
    } else {
      throw new IllegalTransformationException("Unsupported target " +
          Context.get().getTarget(), _claw.getPragma().lineNo());
    }

    if(!_fctType.getBooleanAttribute(Xattr.IS_PRIVATE)) {
      FmoduleDefinition modDef = _fctDef.findParentModule();
      if(modDef != null) {
        Xmod.updateSignature(modDef.getName(), xcodeml, _fctDef, _fctType,
            false);
      }
    }
  }

  /**
   * Apply GPU based transformation.
   *
   * @param xcodeml Current XcodeML program unit.
   */
  private void transformForGPU(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    // TODO nodep should be passed in another way.
    int collapse = Directive.generateLoopSeq(xcodeml, _fctDef,
        CompilerDirective.CLAW.getPrefix() + " nodep");

    if(!Body.isEmpty(_fctDef.body())) {
      /* Create a nested loop with the new defined dimensions and wrap it around
       * the whole subroutine's body. This is for the moment a really naive
       * transformation idea but it is our start point.
       * Use the first over clause to create it. */
      NestedDoStatement loops =
          new NestedDoStatement(_claw.getDimensionValuesReversed(), xcodeml);

      /* Subroutine/function can have a contains section with inner subroutines or
       * functions. The newly created (nested) do statements should stop before
       * this contains section if it exists. */
      Xnode contains = _fctDef.body().matchSeq(Xcode.F_CONTAINS_STATEMENT);
      if(contains != null) {

        Xnode parallelRegionStart =
            Directive.findParallelRegionStart(_fctDef, null);
        Xnode parallelRegionEnd =
            Directive.findParallelRegionEnd(_fctDef, contains);

        Body.shiftIn(parallelRegionStart, parallelRegionEnd,
            loops.getInnerStatement().body(), true);

        contains.insertBefore(loops.getOuterStatement());
      } else {
        // No contains section, all the body is copied to the do statements.

        Xnode parallelRegionStart =
            Directive.findParallelRegionStart(_fctDef, null);
        Xnode parallelRegionEnd =
            Directive.findParallelRegionEnd(_fctDef, null);

        // Define a hook from where we can insert the new do statement
        Xnode hook = parallelRegionEnd != null
            ? parallelRegionEnd.nextSibling() : null;
        Body.shiftIn(parallelRegionStart, parallelRegionEnd,
            loops.getInnerStatement().body(), true);

        // Hook is null then we append the do statement to the current fct body
        if(hook == null) {
          _fctDef.body().append(loops.getOuterStatement());
        } else {
          // Insert new do statement before the hook element
          hook.insertBefore(loops.getOuterStatement());
        }
      }

      // Prepare variables list for present/pcreate clauses and handle
      // promotion/privatize local strategy
      List<String> presentList =
          Directive.getPresentVariables(xcodeml, _fctDef);
      List<String> privateList = Collections.emptyList();
      List<String> createList = Collections.emptyList();
      if(Configuration.get().openACC().getLocalStrategy()
          == OpenAccLocalStrategy.PRIVATE)
      {
        privateList = Directive.getLocalArrays(xcodeml, _fctDef);
        // Iterate over a copy to be able to remove items
        for(String identifier : new ArrayList<>(privateList)) {
          if(_promotions.containsKey(identifier)) {
            privateList.remove(identifier);
          }
        }
      } else if(Configuration.get().openACC().getLocalStrategy()
          == OpenAccLocalStrategy.PROMOTE)
      {
        createList = Directive.getLocalArrays(xcodeml, _fctDef);
        for(String arrayIdentifier : createList) {
          _arrayFieldsInOut.add(arrayIdentifier);
          PromotionInfo promotionInfo = new PromotionInfo(arrayIdentifier,
              _claw.getDimensionsForData(arrayIdentifier));
          Field.promote(promotionInfo, _fctDef, xcodeml);
          _promotions.put(arrayIdentifier, promotionInfo);

          Field.adaptArrayRef(promotionInfo, _fctDef.body(), xcodeml);
          Field.adaptAllocate(promotionInfo, _fctDef.body(), xcodeml);
        }
      }

      // Generate the data region
      Directive.generateDataRegionClause(xcodeml, presentList,
          createList, loops.getOuterStatement(), loops.getOuterStatement());

      // Generate the parallel region
      Directive.generateParallelLoopClause(xcodeml, privateList,
          loops.getOuterStatement(), loops.getOuterStatement(),
          loops.size() + collapse);
    }

    Directive.generateRoutineDirectives(xcodeml, _fctDef);
  }

  /**
   * Apply a CPU transformation which group adjacent statements together and
   * promote the minimum number of variables possible.
   *
   * @param xcodeml Current XcodeML program unit.
   * @throws IllegalTransformationException If promotion of arrays fails.
   */
  private void transformPartialForCPU(XcodeProgram xcodeml)
          throws IllegalTransformationException {
    // Apply transformation only when there is a body to modify
    if (Body.isEmpty(_fctDef.body())) {
      return;
    }
    // Variables on each "depth" of the method
    List<Set<String>> depthVars = new ArrayList<>();
    depthVars.add(new HashSet<String>());
    // A growing list of affecting variables. Starting from the ones
    // declared by SCA and successively any variables affected by the SCA
    // declaration
    Set<String> affectingVars = new HashSet<>(_promotions.keySet());
    affectingVars.addAll(_arrayFieldsInOut);
    // Keep track of the block we are in
    int depth = 0;

    // Extract all the variables used, this doesn't include vector's
    // iterator variables
    transformPartialForCPUExtraction(_fctDef.body(), depth, depthVars,
            affectingVars);

    // Find the block we need to transform
    List<Set<String>> targetDepthIntersections = new ArrayList<>();
    int transformationNumbers = 0;
    for (int i = 0; i < depthVars.size(); i++) {
      Set<String> depthVar = depthVars.get(i);

      if (depthVar.isEmpty()) {
        targetDepthIntersections.add(null);
        continue;
      }
      // Find intersection with other blocks
      Set<String> intersection = new HashSet<>();
      for (int j = 0; j < depthVars.size(); j++) {
        // Skip itself
        if (i == j) {
          continue;
        }
        // Intersect the level set with the others
        Set<String> copy = new HashSet<>(depthVars.get(i));
        copy.retainAll(depthVars.get(j));
        intersection.addAll(copy);
        transformationNumbers++;
      }

      // All are on the same index
      targetDepthIntersections.add(intersection);
    }

    // No transformation found, abort
    if (transformationNumbers == 0) {
      Message.debug("No target found for transformation found");
      return;
    }

    // Gather transformation groups
    List<List<Xnode>> transformations = new ArrayList<>();
    // Apply transformation at the indicated depth
    for (int targetDepth = 0; targetDepth < depthVars.size(); targetDepth++) {
      Set<String> vars = depthVars.get(targetDepth);
      if (vars.isEmpty()) continue;
      Set<String> intersection = targetDepthIntersections.get(targetDepth);

      // Variables still waiting for a promotion which aren't promoted yet
      Set<String> promotions = new HashSet<>(vars);
      promotions.retainAll(intersection);

      // Influenced arrays must be promoted anyway
      for (String var : vars) {
        Xid fieldId = _fctDef.getSymbolTable().get(var);
        FbasicType type = xcodeml.getTypeTable().getBasicType(fieldId);
        if (type != null && type.isArray()) {
          promotions.add(var);
        }
      }

      // Promote
      for (String promotion : promotions) {
        if (_promotions.containsKey(promotion)) {
            continue;
        }
        PromotionInfo promotionInfo = new PromotionInfo(promotion,
                _claw.getDimensionsForData(promotion));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(promotion, promotionInfo);
        Field.adaptArrayRef(promotionInfo, _fctDef.body(), xcodeml);
        Field.adaptAllocate(_promotions.get(promotion), _fctDef.body(),
                xcodeml);
      }

      // Transform indicated level by wrapping it in a DO loop
      transformPartialForCPUGather(_fctDef.body(), 0, targetDepth,
              affectingVars, transformations);
    }

    // After gathering we apply the transformations
    for (List<Xnode> transformation : transformations) {
      transformPartialForCPUApply(transformation, xcodeml);
    }
    // Generate the parallel region
    Directive.generateParallelClause(xcodeml, _fctDef.body().firstChild(),
            _fctDef.body().lastChild());
  }

  /**
   * Analysis of a routine in order to extracts the information necessary in
   * order to promote a minimal number of variables.
   * Only variables affected by affectingVars are kept in depthVars.
   *
   * @param body The body to analyze.
   * @param depth The depth relative to the function declaration.
   * @param depthVars Vars used on each depth.
   * @param affectingVars Vars which are affected by SCA and consequently
   *                      affect other variables.
   */
  private void transformPartialForCPUExtraction(Xnode body, int depth,
                                                List<Set<String>> depthVars,
                                                Set<String> affectingVars) {
    final List<Xnode> children = body.children();
    for (Xnode node : children) {
      // Handle an assignment
      if (node.opcode() == Xcode.F_ASSIGN_STATEMENT) {
        AssignStatement as = new AssignStatement(node.element());
        Set<String> vars = XnodeUtil.findChildrenVariables(node);

        // Check if it's affected by the promotion
        Set<String> affectedVars = new HashSet<>(vars);
        affectedVars.retainAll(affectingVars);

        // If the intersection of the sets contain anything the assignment is
        // affected by a previous promotions
        depthVars.get(depth).addAll(affectedVars);
        if (!affectedVars.isEmpty()) {
          affectingVars.add(as.getLhsName());
          depthVars.get(depth).add(as.getLhsName());
        }
      }
      // IF statement content shouldn't increase depth counter
      else if (node.opcode() == Xcode.F_IF_STATEMENT) {
        transformPartialForCPUExtraction(node.lastChild().body(), depth,
                depthVars, affectingVars);
      }
      // Handle node containing a body
      else if (node.opcode().hasBody()) {
        if (depthVars.size() <= depth + 1) {
          depthVars.add(new HashSet<String>());
        }
        transformPartialForCPUExtraction(node.body(), depth + 1, depthVars,
                affectingVars);
      }
      // Keep going inside the new node
      else {
        transformPartialForCPUExtraction(node, depth, depthVars,
                affectingVars);
      }
    }
  }

  /**
   * Transform the content of the routine and add a DO loop only at the
   * indicated depth and only around the affecting variables.
   *
   * @param body The body to transform.
   * @param targetDepth The depth we need to reach and transform.
   * @param affectingVars The variable which should be contained inside the loop
   */
  private void transformPartialForCPUGather(Xnode body, int currentDepth,
                                            int targetDepth,
                                            Set<String> affectingVars,
                                            List<List<Xnode>> toapply) {
    final List<Xnode> children = body.children();
    final List<Xnode> hooks = new ArrayList<>();
    for (Xnode node : children) {
      // Handle an assignment
      if (node.opcode() == Xcode.F_ASSIGN_STATEMENT) {
        if (currentDepth != targetDepth) continue;
        Set<String> vars = XnodeUtil.findChildrenVariables(node);
        // Statement need to be in the loop
        if (Utility.hasIntersection(vars, affectingVars)) {
          // Is the statement wasn't promoted and is not in a current loop
          // block, we can skip the promotion because it is not needed.
          AssignStatement as = new AssignStatement(node.element());
          if (!_promotions.containsKey(as.getLhsName()) && hooks.isEmpty()) {
              continue;
          }
          // If the assignment is a vector, but we don't access its elements
          // we don't have to add it to a loop
          if (as.getVarRefNames().size() == 0) {
            transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
            continue;
          }
          hooks.add(node);
        }
        // Particular case, unused variable inside the body
        else {
          transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
        }
      }
      // IF statement my have to be contained inside
      else if (node.opcode() == Xcode.F_IF_STATEMENT) {
        if (currentDepth != targetDepth) continue;
        Set<String> vars = XnodeUtil.findChildrenVariables(node.firstChild());
        // An assign statement is needed inside the IF for it to be eligible
        final boolean varsIntersection = Utility.hasIntersection(vars,
                affectingVars);
        final boolean assignExists = node.matchDescendant(Xcode
                .F_ASSIGN_STATEMENT) != null;
        if (varsIntersection) {
          if (assignExists) {
            hooks.add(node);
          } else {
            transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
          }
        }
        // Continue inside if
        else {
          transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
          transformPartialForCPUGather(node.lastChild().body(), currentDepth,
                  targetDepth, affectingVars, toapply);
        }
      }
      // Handle node containing a body
      else if (node.opcode().hasBody()) {
        transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
        transformPartialForCPUGather(node.body(), currentDepth + 1,
                targetDepth, affectingVars, toapply);
      }
      // Keep going inside the new node
      else {
        transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
        transformPartialForCPUGather(node, currentDepth,
                targetDepth, affectingVars, toapply);
        }
    }
    transformPartialForCPUGatherSaveHooksGroup(hooks, toapply);
  }

  /**
   * If a hooks group that will be wrapped exists, add it to the collection
   * containing all the hooks group found until now.
   * The content of `hooks` is copied in a new list and added to `toapply`,
   * while `hooks` will be cleared of its content.
   *
   * @param hooks A list of adjacent nodes to be wrapped in a DO statement
   * @param toapply A list to add a copy of `hooks`
   */
  private void transformPartialForCPUGatherSaveHooksGroup(List<Xnode> hooks,
                                                          List<List<Xnode>>
                                                                  toapply) {
    if (!hooks.isEmpty()) {
      toapply.add(new ArrayList<>(hooks));
      hooks.clear();
    }
  }

  /**
   * Given a series of sequential hooks (nodes), envelope them in a DO loop.
   *
   * @param hooks List of nodes do envelope in a DO statement
   * @param xcodeml The program
   */
  private void transformPartialForCPUApply(List<Xnode> hooks,
                                           XcodeProgram xcodeml) {
    if (hooks.isEmpty()) {
        return;
    }
    NestedDoStatement loop =
            new NestedDoStatement(_claw.getDimensionValuesReversed(), xcodeml);
    // Add loop to AST
    hooks.get(0).insertBefore(loop.getOuterStatement());
    for(Xnode hook : hooks) {
      loop.getInnerStatement().body().append(hook, false);
    }
    Directive.generateLoopDirectives(xcodeml,
            loop.getOuterStatement(), loop.getOuterStatement(),
            Directive.NO_COLLAPSE);
    hooks.clear();
  }

  /**
   * Promote all fields declared in the data clause with the additional
   * dimensions.
   *
   * @param xcodeml Current XcodeML program unit in which the element will be
   *                created.
   * @throws IllegalTransformationException if elements cannot be created or
   *                                        elements cannot be found.
   */
  private void promoteFields(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    if(_claw.hasOverDataClause()) {
      for(int i = 0; i < _claw.getOverDataClauseValues().size(); ++i) {
        for(String fieldId : _claw.getOverDataClauseValues().get(i)) {
          PromotionInfo promotionInfo = new PromotionInfo(fieldId,
              _claw.getDimensionsForData(fieldId));
          Field.promote(promotionInfo, _fctDef, xcodeml);
          _promotions.put(fieldId, promotionInfo);
        }
      }
    } else {
      // Promote all arrays in a similar manner
      for(String fieldId : _arrayFieldsInOut) {
        PromotionInfo promotionInfo = new PromotionInfo(fieldId,
            _claw.getDimensionsForData(fieldId));
        Field.promote(promotionInfo, _fctDef, xcodeml);
        _promotions.put(fieldId, promotionInfo);
      }
    }
  }

  /**
   * Insert the declaration of the different variables needed to iterate over
   * the additional dimensions.
   *
   * @param xcodeml Current XcodeML program unit in which element are
   *                created.
   */
  private void insertVariableToIterateOverDimension(XcodeProgram xcodeml) {
    // Create type and declaration for iterations over the new dimensions
    FbasicType bt = xcodeml.createBasicType(FortranType.INTEGER, Intent.IN);
    xcodeml.getTypeTable().add(bt);

    // For each dimension defined in the directive
    for(DimensionDefinition dimension : _claw.getDimensionValues()) {
      // Create the parameter for the lower bound
      if(dimension.getLowerBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getLowerBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef,
            DeclarationPosition.FIRST);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getLowerBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }

      // Create parameter for the upper bound
      if(dimension.getUpperBound().isVar()) {
        xcodeml.createIdAndDecl(dimension.getUpperBound().getValue(),
            bt.getType(), XstorageClass.F_PARAM, _fctDef,
            DeclarationPosition.FIRST);

        // Add parameter to the local type table
        Xnode param = xcodeml.createAndAddParam(
            dimension.getUpperBound().getValue(),
            bt.getType(), _fctType);
        param.setBooleanAttribute(Xattr.IS_INSERTED, true);
      }
      // Create induction variable declaration
      xcodeml.createIdAndDecl(dimension.getIdentifier(), FortranType.INTEGER,
          XstorageClass.F_LOCAL, _fctDef, DeclarationPosition.LAST);
    }
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation other)
  {
    return false; // This is an independent transformation
  }
}
