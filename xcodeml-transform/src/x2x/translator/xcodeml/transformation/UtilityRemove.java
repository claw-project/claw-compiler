package x2x.translator.xcodeml.transformation;

import x2x.translator.pragma.CLAWpragma;
import x2x.translator.xcodeml.xelement.*;
import x2x.translator.xcodeml.transformer.Transformer;

public class UtilityRemove extends Transformation<UtilityRemove> {

  // The loop statement involved in the Transformation
  private XdoStatement _loop = null;
  private Xpragma _end = null;

  public UtilityRemove(Xpragma pragma){
    super(pragma);
  }

  public void setEnd(Xpragma pragma){
    _end = pragma;
  }

  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    // Check if there is an end pragma

    // if not, check if the next block is an if statement or a do statement

    return true;
  }


  public void transform(XcodeProg xcodeml, Transformer transformer,
    UtilityRemove other)
  {
    // Delete all
  }

  public boolean canBeTransformedWith(UtilityRemove other){
    return true; // Always true as independent transformation
  }

}
