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
public class UtilityRemove extends Transformation<UtilityRemove> {

  // The loop statement involved in the Transformation
  private XdoStatement _do = null;
  private XifStatement _if = null;
  private Xpragma _end = null;

  /**
   * Constructs a new UtilityRemove triggered from a specific pragma.
   * @param directive The directive that triggered the remove transformation.
   */
  public UtilityRemove(ClawLanguage directive){
    super(directive);
  }

  public void setEnd(Xpragma pragma){
    _end = pragma;
  }

  public boolean analyze(XcodeProgram xcodeml, Transformer transformer) {

    // if there is no end directive, the following statement must be a if or
    // do statement
    if(_end == null){
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
    if(_end == null){
      if(_do != null){
        _do.delete();
      } else if(_if != null){
        _if.delete();
      }
      _directive.getPragma().delete();
    } else {
      XelementHelper.deleteBetween(_directive.getPragma(), _end);
      _directive.getPragma().delete();
      _end.delete();
    }
  }

  public boolean canBeTransformedWith(UtilityRemove other){
    return true; // Always true as independent transformation
  }

}
