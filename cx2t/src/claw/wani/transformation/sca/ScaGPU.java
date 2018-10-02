/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.CompilerDirective;
import claw.tatsu.directive.common.Directive;
import claw.tatsu.primitive.Body;
import claw.tatsu.primitive.Field;
import claw.tatsu.xcodeml.abstraction.NestedDoStatement;
import claw.tatsu.xcodeml.abstraction.PromotionInfo;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.language.ClawPragma;
import claw.wani.x2t.configuration.AcceleratorConfiguration;
import claw.wani.x2t.configuration.AcceleratorLocalStrategy;
import claw.wani.x2t.configuration.Configuration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Specialized version of SCA transformation for GPU target.
 *
 * Transformation for the GPU target: <ul>
 * <li> Automatic promotion is applied to all arrays with intent in, out or
 * inout.
 * <li> Do statements over the additional dimensions is added as an outer
 * loop and wrap the entire body of the subroutine.
 * </ul>
 *
 * Generation of OpenACC directives:<ul>
 * <li> acc routine seq is generated for subroutine called from the SCA
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
 * Generation of OpenMP directives on CPU: <ul>
 * <li> omp parallel do is generated for each generated do statements.
 * </ul>
 *
 * Generation of OpenMP directives on GPU:<ul>
 * <li> MISSING FEATURE : omp declare target is generated for subroutine called
 * from the SCA subroutine if they are located in the same translation unit.
 * <li> omp data region with corresponding present clause for all promoted
 * variables with the intent to, from or tofrom.
 * <li> omp target teams distribute region is generated to wrap all the body of
 * the subroutine.
 * <li> omp private clause is added to the target directive for all local
 * variables.
 * <li> omp collapse is generated for the generated do statement
 * (if more that 1).
 * </ul>
 *
 * @author clementval
 */
public class ScaGPU extends Sca {

  /**
   * Constructs a new SCA transformation triggered from a specific
   * pragma for a GPU target.
   *
   * @param directive The directive that triggered the define transformation.
   */
  public ScaGPU(ClawPragma directive) {
    super(directive);
  }

  @Override
  public void transform(XcodeProgram xcodeml, Translator translator,
                        Transformation other)
      throws Exception
  {
    // Apply the common transformation
    super.transform(xcodeml, translator, other);

    // Apply specific steps for CPU smart fusion
    applySpecificTransformation(xcodeml);

    // Finalize the common steps
    super.finalizeTransformation(xcodeml);
  }

  /**
   * Apply specific transformation steps for GPU target.
   *
   * @param xcodeml Current translation unit.
   * @throws IllegalTransformationException If any transformation fails.
   */
  private void applySpecificTransformation(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    AcceleratorConfiguration config = Configuration.get().accelerator();

    // TODO nodep passing!
    int collapse = Directive.generateLoopSeq(xcodeml, _fctDef,
        CompilerDirective.CLAW.getPrefix() + " nodep");

    if(_fctDef.hasEmptyBody()) {
      return; // Nothing to do in this function
    }

    /* Create a nested loop with the new defined dimensions and wrap it around
     * the whole subroutine's body. This is for the moment a really naive
     * transformation idea but it is our start point.
     * Use the first over clause to create it. */
    NestedDoStatement loops =
        new NestedDoStatement(_claw.getDefaultLayoutReversed(), xcodeml);

    /* Subroutine/function can have a contains section with inner subroutines
     * or functions. The newly created (nested) do statements should stop
     * before this contains section if it exists. */
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
    if(config.getLocalStrategy() == AcceleratorLocalStrategy.PRIVATE) {
      privateList = applyPrivateStrategy(xcodeml);
    } else if(config.getLocalStrategy() == AcceleratorLocalStrategy.PROMOTE) {
      createList = applyPromoteStrategy(xcodeml);
    }

    // Generate the data region
    Directive.generateDataRegionClause(xcodeml, presentList,
        createList, loops.getOuterStatement(), loops.getOuterStatement());

    // Generate the parallel region
    Directive.generateParallelLoopClause(xcodeml, privateList,
        loops.getOuterStatement(), loops.getOuterStatement(),
        loops.size() + collapse);

    Directive.generateRoutineDirectives(xcodeml, _fctDef);
  }

  /**
   * Apply the private local array strategy. Gather all information about
   * local array requiring a privatization.
   *
   * @param xcodeml Current translation unit.
   * @return List of private variables.
   */
  private List<String> applyPrivateStrategy(XcodeProgram xcodeml) {
    List<String> privateList = Directive.getLocalArrays(xcodeml, _fctDef);
    // Iterate over a copy to be able to remove items
    for(String identifier : new ArrayList<>(privateList)) {
      if(_promotions.containsKey(identifier)) {
        privateList.remove(identifier);
      }
    }
    return privateList;
  }

  /**
   * Apply the promotion local array strategy. Gather all information about
   * local variable requiring a promotion and apply it.
   *
   * @param xcodeml Current translation unit.
   * @return List of promoted variable requiring an allocation.
   * @throws IllegalTransformationException If promotion of variable fails.
   */
  private List<String> applyPromoteStrategy(XcodeProgram xcodeml)
      throws IllegalTransformationException
  {
    List<String> createList = Directive.getLocalArrays(xcodeml, _fctDef);
    for(String arrayIdentifier : createList) {
      _arrayFieldsInOut.add(arrayIdentifier);
      PromotionInfo promotionInfo = new PromotionInfo(arrayIdentifier,
          _claw.getLayoutForData(arrayIdentifier));

      Field.promote(promotionInfo, _fctDef, xcodeml);
      _promotions.put(arrayIdentifier, promotionInfo);

      Field.adaptArrayRef(promotionInfo, _fctDef.body(), xcodeml);
      Field.adaptAllocate(promotionInfo, _fctDef.body(), xcodeml);
    }
    return createList;
  }

}
