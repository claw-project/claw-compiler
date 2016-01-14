package cx2x.translator.xcodeml.transformer;

import cx2x.translator.xcodeml.transformation.*;
import java.util.List;

/**
 * Transformer interface
 *
 * Transformer stores all the transformation to be applied by a translator.
 *
 * @author Valentin Clement
 */

public interface Transformer {
  public void addTransformation(Transformation t);
  public void addTransformationGroup(TransformationGroup tg, int position);
  public List<TransformationGroup> getGroups();
}
