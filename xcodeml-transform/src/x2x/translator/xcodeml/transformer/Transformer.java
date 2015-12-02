package x2x.translator.xcodeml.transformer;

import x2x.translator.xcodeml.transformation.*;

import java.util.List;

public interface Transformer {
  public void addTransformation(Transformation t);
  public void addTransformationGroup(TransformationGroup tg, int position);
  public List<TransformationGroup> getGroups();
}
