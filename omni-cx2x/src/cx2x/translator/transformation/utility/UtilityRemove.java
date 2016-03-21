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

/**
 * A UtilityRemove is an independent transformation. It allows to delete part of
 * the code.
 *
 * @author clementval
 */
public class UtilityRemove extends BlockTransformation<UtilityRemove> {

  // The loop statement involved in the Transformation
  private XdoStatement _do = null;
  private XifStatement _if = null;

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
  }

  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // if there is no end directive, the following statement must be a if or
    // do statement
    if(_endDirective == null){
      _do = XelementHelper.findDirectNextDoStmt(_directive.getPragma());
      _if = XelementHelper.findDirectNextIfStmt(_directive.getPragma());

      if(_do == null && _if == null){
        xcodeml.addError("Directive remove without end not followed by a do or if statement",
          _directive.getPragma().getLineNo());
        return false;
      }
    }
    return true;
  }


  public void transform(XcodeProgram xcodeml, Transformer transformer,
                        UtilityRemove other) throws IllegalTransformationException
  {
    if(_endDirective == null){
      if(_do != null){
        _do.delete();
      } else if(_if != null){
        _if.delete();
      }
      _directive.getPragma().delete();
    } else {
      XelementHelper.deleteBetween(_directive.getPragma(),
          _endDirective.getPragma());
      _directive.getPragma().delete();
      _endDirective.getPragma().delete();
    }
  }

  public boolean canBeTransformedWith(UtilityRemove other){
    return true; // Always true as independent transformation
  }

}
