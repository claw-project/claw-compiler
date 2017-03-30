/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.loop;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.language.helper.accelerator.AcceleratorHelper;
import cx2x.translator.transformation.ClawTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;
import xcodeml.util.XmOption;

import java.util.List;

/**
 * A LoopInterchange transformation is a an independent transformation. It allow
 * to reorder nested loops up to three levels.
 *
 * @author clementval
 */

public class LoopInterchange extends ClawTransformation {

  private List<String> _newOrderOption = null;
  private Xnode _loopLevel0 = null;
  private Xnode _loopLevel1 = null;
  private Xnode _loopLevel2 = null;
  private String _baseLoop0 = null;
  private String _baseLoop1 = null;
  private String _baseLoop2 = null;
  private String _newLoop0 = null;
  private String _newLoop1 = null;
  private String _newLoop2 = null;
  // New ordering of the loops. Values are initial position.
  private int _loopNewPos0 = 0;
  private int _loopNewPos1 = 1;
  private int _loopNewPos2 = 2;

  /**
   * Constructs a new LoopInterchange triggered from a specific pragma.
   *
   * @param directive The directive that triggered the loop interchange
   *                  transformation.
   */
  public LoopInterchange(ClawLanguage directive) {
    super(directive);
    _newOrderOption = directive.getIndexes();
  }

  /**
   * Apply the transformation.
   *
   * @param xcodeml        The XcodeML on which the transformations are applied.
   * @param transformer    The transformer used to applied the transformations.
   * @param transformation Only for dependent transformation. The other
   *                       transformation part of the transformation.
   * @throws IllegalTransformationException if the transformation cannot be
   *                                        applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {

    analyze(xcodeml, transformer);

    // TODO
  /*  if(XmOption.isDebugOutput()){
      System.out.println("loop-interchange transformation");
      System.out.println("  loop 0: " + _loopLevel0.getFormattedRange());
      System.out.println("  loop 1: " + _loopLevel1.getFormattedRange());
      if(_loopLevel2 != null){
        System.out.println("  loop 2: " + _loopLevel2.getFormattedRange());
      }
    }*/

    /* To perform the loop interchange, only the ranges and iteration
     * variables are swapped */
    if(_loopLevel1 != null && _loopLevel2 == null) {
      // Loop interchange between 2 loops
      XnodeUtil.swapIterationRange(_loopLevel0, _loopLevel1);
    } else {
      // loop interchange between 3 loops with new-order
      computeLoopNewPosition();
      printTransformDebugInfo();

      if(needDoubleSwap()) {
        // Case 201
        if(_loopNewPos0 == 2 && _loopNewPos1 == 0 && _loopNewPos2 == 1) {
          printTransformSwapInfo(201);
          XnodeUtil.swapIterationRange(_loopLevel0, _loopLevel2);
          XnodeUtil.swapIterationRange(_loopLevel0, _loopLevel1);
          // Case 120
        } else if(_loopNewPos0 == 1 && _loopNewPos1 == 2 && _loopNewPos2 == 0) {
          printTransformSwapInfo(120);
          XnodeUtil.swapIterationRange(_loopLevel0, _loopLevel2);
          XnodeUtil.swapIterationRange(_loopLevel1, _loopLevel2);
        }
      } else {
        // Only one loop swap is needed
        Xnode from = null;
        Xnode to = null;
        if(_loopNewPos0 == 0) { // Loop 0 stay in place 0
          from = _loopLevel1;
          to = _loopLevel2;
        } else if(_loopNewPos1 == 1) { // Loop 1 stay in place 1
          from = _loopLevel0;
          to = _loopLevel2;
        } else if(_loopNewPos2 == 2) { // Loop 2 stay in place 2
          from = _loopLevel0;
          to = _loopLevel1;
        }
        XnodeUtil.swapIterationRange(from, to);
      }
    }

    // Generate accelerator pragmas if needed
    AcceleratorHelper.
        generateAdditionalDirectives(_claw, xcodeml, _loopLevel0, _loopLevel0);

    _claw.getPragma().delete();

    this.transformed();
  }

  /**
   * Check whether the transformation needs a single swap transformation or a
   * double swap transformation.
   *
   * @return True if the transformation needs a double swap. False if a single
   * swap is needed.
   */
  private boolean needDoubleSwap() {
    return (_loopNewPos0 == 2 && _loopNewPos1 == 0 && _loopNewPos2 == 1) ||
        (_loopNewPos0 == 1 && _loopNewPos1 == 2 && _loopNewPos2 == 0);
  }

  /**
   * Based on the new ordering option, compute the new position of the different
   * do statement.
   */
  private void computeLoopNewPosition() {
    if(_baseLoop0.equals(_newLoop1)) {
      _loopNewPos0 = 1;
    } else if(_baseLoop0.equals(_newLoop2)) {
      _loopNewPos0 = 2;
    }

    if(_baseLoop1.equals(_newLoop0)) {
      _loopNewPos1 = 0;
    } else if(_baseLoop1.equals(_newLoop2)) {
      _loopNewPos1 = 2;
    }

    if(_baseLoop2.equals(_newLoop0)) {
      _loopNewPos2 = 0;
    } else if(_baseLoop2.equals(_newLoop1)) {
      _loopNewPos2 = 1;
    }
  }

  /**
   * Loop fusion analysis:
   * - Find the different do statement that will be reordered.
   * - Check the validity of the new ordering option.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True if the transformation can be performed. False otherwise.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // Find next loop after pragma
    _loopLevel0 = _claw.getPragma().matchSibling(Xcode.FDOSTATEMENT);

    if(_loopLevel0 == null) {
      xcodeml.addError("top level loop not found",
          _claw.getPragma().lineNo());
      return false;
    }

    _loopLevel1 = _loopLevel0.body().matchDirectDescendant(Xcode.FDOSTATEMENT);
    if(_loopLevel1 == null) {
      return false;
    }

    if(_newOrderOption != null) {
      if(_newOrderOption.size() != 3) {
        xcodeml.addError("new-order option has not enough parameters",
            _claw.getPragma().lineNo());
      }

      _loopLevel2 =
          _loopLevel1.body().matchDirectDescendant(Xcode.FDOSTATEMENT);

      if(_loopLevel2 == null) {
        return false;
      }

      _baseLoop0 = _loopLevel0.matchSeq(Xcode.VAR).value();
      _baseLoop1 = _loopLevel1.matchSeq(Xcode.VAR).value();
      _baseLoop2 = _loopLevel2.matchSeq(Xcode.VAR).value();

      if(!checkNewOrderOption(xcodeml, _newOrderOption)) {
        return false;
      }

      _newLoop0 = _newOrderOption.get(0);
      _newLoop1 = _newOrderOption.get(1);
      _newLoop2 = _newOrderOption.get(2);
    }

    return true;
  }

  /**
   * Check the validity of the new ordering option.
   *
   * @param xcodeml The XcodeML object.
   * @param idxs    List containing the induction variables.
   * @return True if the new ordering is valid. False otherwise.
   */
  private boolean checkNewOrderOption(XcodeProgram xcodeml, List<String> idxs) {
    for(String idx : idxs) {
      if(!idx.equals(_baseLoop0) && !idx.equals(_baseLoop1)
          && !idx.equals(_baseLoop2))
      {
        xcodeml.addError("invalid induction variable in new-order option. "
            + idx, _claw.getPragma().lineNo());
        return false;
      }
    }
    return true;
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    // independent transformation
    return false;
  }

  /**
   * Print some useful debugging information
   */
  private void printTransformDebugInfo() {
    if(XmOption.isDebugOutput()) {
      System.out.println("  transform from " + _baseLoop0 + "," + _baseLoop1
          + "," + _baseLoop2 + " (012) to " + _newLoop0 + "," + _newLoop1 +
          "," + _newLoop2 + " (" + _loopNewPos0 + _loopNewPos1 +
          _loopNewPos2 + ")");

      if(needDoubleSwap()) {
        System.out.println("    double swap required");
      }
    }
  }

  /**
   * Print information for double swap cases
   *
   * @param swapCase Integer representing the new ordering (120 or 201)
   */
  private void printTransformSwapInfo(int swapCase) {
    if(XmOption.isDebugOutput() && swapCase == 120) {
      System.out.println("    swap 1: " + _baseLoop0 + " <--> " + _baseLoop2);
      System.out.println("    swap 2: " + _baseLoop1 + " <--> " + _baseLoop0);
    } else if(XmOption.isDebugOutput() && swapCase == 201) {
      System.out.println("    swap 1: " + _baseLoop0 + " <--> " + _baseLoop2);
      System.out.println("    swap 2: " + _baseLoop2 + " <--> " + _baseLoop1);
    }
  }


}
