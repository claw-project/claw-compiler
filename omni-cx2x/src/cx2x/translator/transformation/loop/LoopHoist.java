/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.XelementHelper;
import cx2x.xcodeml.transformation.BlockTransformation;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xelement.XbaseElement;
import cx2x.xcodeml.xelement.XcodeProgram;
import cx2x.xcodeml.xelement.XdoStatement;

import java.util.ArrayList;
import java.util.List;

/**
 * A LoopHoist transformation is an independent transformation over a
 * structured block. It performs loop fusion in the given block as well as do
 * start statement hoisting.
 *
 * @author clementval
 */
public class LoopHoist extends BlockTransformation {

  private List<XdoStatement[]> _loops;
  private int _nestedLevel;
  private ClawLanguage _startClaw, _endClaw;

  /**
   * Constructs a new LoopHoist triggered from a specific directive.
   * @param startDirective The directive that triggered the loop hoist
   *                       transformation.
   * @param endDirective   The directive that end the structured block.
   */
  public LoopHoist(ClawLanguage startDirective, ClawLanguage endDirective) {
    super(startDirective, endDirective);
    _startClaw = startDirective;
    _endClaw = endDirective;
    _loops = new ArrayList<>();
  }

  /**
   * @see Transformation#analyze(XcodeProgram, Transformer)
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    _nestedLevel = _startClaw.getHoistInductionVars().size();

    XdoStatement outterLoop;
    XbaseElement from = _startClaw.getPragma();
    do {
      outterLoop = XelementHelper.
          findNextDoStatement(from, _endClaw.getPragma());
      if(outterLoop.getInductionVariable().equals(_startClaw.getHoistInductionVars().get(0))){
        // outter loop meet induction variable criteria

      }
      from = outterLoop;  // next search start from the current loop
    } while(outterLoop != null);

    return true;
  }

  /**
   * @see Transformation#canBeTransformedWith(Transformation)
   */
  @Override
  public boolean canBeTransformedWith(Transformation transformation) {
    return false;
  }

  /**
   * @see Transformation#transform(XcodeProgram, Transformer, Transformation)
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation) throws Exception
  {

  }

}
