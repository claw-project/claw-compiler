package x2x.translator.xcodeml.translation;

import x2x.translator.xcodeml.xelement.XcodeProg;

public interface Translation {

  public boolean analyze();
  public void transform(XcodeProg xcodeml);

}
