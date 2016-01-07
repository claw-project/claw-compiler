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

  public boolean analyze(XcodeProg xcodeml, Transformer transformer) {
    // Check if there is an end pragma
    return true;
  }


  public void transform(XcodeProg xcodeml, Transformer transformer,
    UtilityRemove other)
  {
  }

  public boolean canBeTransformedWith(UtilityRemove other){
    return true; // Always true as independent transformation
  }

}
