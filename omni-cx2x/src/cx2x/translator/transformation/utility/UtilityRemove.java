/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformation.utility;

import cx2x.translator.language.base.ClawLanguage;
import cx2x.translator.transformation.ClawBlockTransformation;
import cx2x.xcodeml.exception.IllegalTransformationException;
import cx2x.xcodeml.helper.XnodeUtil;
import cx2x.xcodeml.transformation.Transformation;
import cx2x.xcodeml.transformation.Transformer;
import cx2x.xcodeml.xnode.Xcode;
import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.xnode.Xnode;

/**
 * A UtilityRemove is an independent transformation. It allows to delete part of
 * the code.
 *
 * @author clementval
 */
public class UtilityRemove extends ClawBlockTransformation {

  // The loop statement involved in the Transformation
  private Xnode _do = null;
  private Xnode _if = null;
  private Xnode _contains = null;

  /**
   * Constructs a new UtilityRemove triggered from a specific pragma.
   *
   * @param startDirective The directive that triggered the remove $
   *                       transformation.
   * @param endDirective   The end directive that close the structured block.
   *                       Can be null if the start directive is used before a
   *                       do statement or an if statement.
   */
  public UtilityRemove(ClawLanguage startDirective, ClawLanguage endDirective) {
    super(startDirective, endDirective);
  }

  /**
   * Check whether the transformation can be applied or not.
   *
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @return True if the transformation can be applied.
   */
  @Override
  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {
    // if there is no end directive, the following statement must be a if or
    // do statement
    if(_clawEnd == null) {
      Xnode next = _clawStart.getPragma().nextSibling();

      _do = next.opcode() == Xcode.FDOSTATEMENT ? next : null;
      _if = next.opcode() == Xcode.FIFSTATEMENT ? next : null;
      _contains = next.opcode() == Xcode.FCONTAINSSTATEMENT ? next : null;

      if(_do == null && _if == null && _contains == null) {
        xcodeml.addError("Directive remove without end not followed by a do " +
            ", if or contains statement", _clawStart.getPragma().lineNo());
        return false;
      }
    }
    return true;
  }

  /**
   * Delete the corresponding elements.
   *
   * @param xcodeml        The XcodeML on which the transformations are applied.
   * @param transformer    The transformer used to applied the transformations.
   * @param transformation Not used for independent transformation.
   * @throws IllegalTransformationException If transformation cannot be applied.
   */
  @Override
  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        Transformation transformation)
      throws IllegalTransformationException
  {
    if(_clawEnd == null) {
      if(_do != null) {
        _do.delete();
      } else if(_if != null) {
        _if.delete();
      } else if(_contains != null) {
        XnodeUtil.deleteFrom(_contains);
      }
      _clawStart.getPragma().delete();
    } else {
      XnodeUtil.deleteBetween(_clawStart.getPragma(), _clawEnd.getPragma());
      _clawStart.getPragma().delete();
      _clawEnd.getPragma().delete();
    }
  }

  /**
   * @return Always false as independent transformation are applied one by one.
   * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
   */
  @Override
  public boolean canBeTransformedWith(XcodeProgram xcodeml,
                                      Transformation transformation)
  {
    return true; // Always true as independent transformation
  }

}
