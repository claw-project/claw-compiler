/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.utility;

import cx2x.translator.language.ClawLanguage;
import cx2x.xcodeml.helper.*;
import cx2x.xcodeml.xelement.*;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.exception.*;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.Xnode;

/**
 * A UtilityRemove is an independent transformation. It allows to delete part of
 * the code.
 *
 * @author clementval
 */
public class UtilityRemove extends BlockTransformation {

  // The loop statement involved in the Transformation
  private Xnode _do = null;
  private Xnode _if = null;
  private Xnode _startPragma = null; // TODO XNODE delete when all move to Xnode
  private Xnode _endPragma = null;   // TODO XNODE delete when all move to Xnode

  private final ClawLanguage _clawStart, _clawEnd;

  /**
   * Constructs a new UtilityRemove triggered from a specific pragma.
   * @param startDirective The directive that triggered the remove $
   *                       transformation.
   * @param endDirective   The end directive that close the structured block.
   *                       Can be null if the start directve is used before a
   *                       do statment or an if statment.
   */
  public UtilityRemove(ClawLanguage startDirective, ClawLanguage endDirective){
    super(startDirective, endDirective);
    _clawStart = startDirective;
    _clawEnd = endDirective;
    _startPragma = new Xnode(_clawStart.getPragma().getBaseElement());
    if(_clawEnd != null) {
      _endPragma = new Xnode(_clawEnd.getPragma().getBaseElement());
    }
  }

  /**
   * Check whether the transformation can be applied or not.
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True if the transformation can be applied.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // if there is no end directive, the following statement must be a if or
    // do statement
    if(_clawEnd == null){
      _do = XelementHelper.findDirectNext(Xcode.FDOSTATEMENT, _startPragma);
      _if = XelementHelper.findDirectNext(Xcode.FIFSTATEMENT, _startPragma);

      if(_do == null && _if == null){
        xcodeml.addError("Directive remove without end not followed by a do " +
            "or if statement", _clawStart.getPragma().getLineNo());
        return false;
      }
    }
    return true;
  }

  /**
   * Delete the corresponding elements.
   * @param xcodeml        The XcodeML on which the transformations are applied.
   * @param transformer    The transformer used to applied the transformations.
   * @param transformation Not used for independant transformation.
   * @throws IllegalTransformationException If transformation cannot be applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    if(_clawEnd == null){
      if(_do != null){
        _do.delete();
      } else if(_if != null){
        _if.delete();
      }
      _clawStart.getPragma().delete();
    } else {
      XelementHelper.deleteBetween(_startPragma, _endPragma);
      _clawStart.getPragma().delete();
      _clawEnd.getPragma().delete();
    }
  }

  /**
   * @see Transformation#canBeTransformedWith(Transformation)
   */
  @Override
  public boolean canBeTransformedWith(Transformation transformation){
    return true; // Always true as independent transformation
  }

}
