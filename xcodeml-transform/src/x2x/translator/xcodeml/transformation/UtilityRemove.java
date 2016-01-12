package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformer.Transformer;

public class UtilityRemove extends Transformation<UtilityRemove> {

  // The loop statement involved in the Transformation
  private XdoStatement _do = null;
  private XifStatement _if = null;
  private Xpragma _end = null;

  public UtilityRemove(Xpragma pragma){
    super(pragma);
  }

  public void setEnd(Xpragma pragma){
    _end = pragma;
  }

  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {

    // if there is no end directive, the following statement must be a if or
    // do statement
    if(_end == null){
      _do = XelementHelper.findDirectNextDoStmt(_pragma);
      _if = XelementHelper.findDirectNextIfStmt(_pragma);

      if(_do == null && _if == null){
        xcodeml.addError("Directive remove without end not followed by a do or if statement",
          _pragma.getLine());
        return false;
      }
    }
    return true;
  }


  public void transform(XcodeProg xcodeml, Transformer transformer,
    UtilityRemove other)
  {
    if(_end == null){
      if(_do != null){
        _do.delete();
      } else if(_if != null){
        _if.delete();
      }
      _pragma.delete();
    } else {
      XelementHelper.deleteBetween(_pragma, _end);
      _pragma.delete();
      _end.delete();
    }
  }

  public boolean canBeTransformedWith(UtilityRemove other){
    return true; // Always true as independent transformation
  }

}
