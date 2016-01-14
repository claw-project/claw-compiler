package cx2x.translator.transformer;

import java.util.ArrayList;
import java.util.List;

import cx2x.translator.transformation.*;

/**
 * ClawTransformer stores all transformation groups applied during the
 * translation.
 *
 * @author Valentin Clement
 */
public class ClawTransformer implements Transformer {
  private TransformationGroup<LoopFusion> _loopFusion = null;
  private TransformationGroup<LoopInterchange> _loopInterchange = null;
  private TransformationGroup<LoopExtraction> _loopExtract = null;
  private TransformationGroup<UtilityRemove> _utilityRemove = null;
  private ArrayList<TransformationGroup> _transformationGroups = null;

  public ClawTransformer(){
    _loopFusion = new DependentTransformationGroup<LoopFusion>("loop-fusion");
    _loopInterchange =
        new IndependentTransformationGroup<LoopInterchange>("loop-interchange");
    _loopExtract =
        new IndependentTransformationGroup<LoopExtraction>("loop-extract");
    _utilityRemove =
        new IndependentTransformationGroup<UtilityRemove>("remove");

    // Add transformations (order of insertion is the one that will be applied)
    _transformationGroups = new ArrayList<TransformationGroup>();
    _transformationGroups.add(_utilityRemove);
    _transformationGroups.add(_loopExtract);
    _transformationGroups.add(_loopFusion);
    _transformationGroups.add(_loopInterchange);
  }

  public void addTransformation(Transformation t){
    if(t instanceof LoopFusion){
      _loopFusion.add((LoopFusion)t);
    } else if(t instanceof LoopInterchange){
      _loopInterchange.add((LoopInterchange)t);
    } else if(t instanceof LoopExtraction){
      _loopExtract.add((LoopExtraction)t);
    } else if(t instanceof UtilityRemove){
      _utilityRemove.add((UtilityRemove)t);
    }
  }

  public void addTransformationGroup(TransformationGroup tg, int position){
    _transformationGroups.add(position, tg);
  }

  public List<TransformationGroup> getGroups(){
    return _transformationGroups;
  }
}
