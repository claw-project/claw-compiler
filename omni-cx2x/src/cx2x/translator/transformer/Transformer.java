package cx2x.translator.transformer;

import cx2x.translator.transformation.*;
import java.util.List;

/**
 * Transformer interface
 *
 * Transformer stores all the transformation to be applied by a translator.
 *
 * @author Valentin Clement
 */

public interface Transformer {
  void addTransformation(Transformation t);
  void addTransformationGroup(TransformationGroup tg, int position);
  List<TransformationGroup> getGroups();
}
