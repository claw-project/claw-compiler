/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformer;

import java.util.ArrayList;
import java.util.List;

import cx2x.translator.transformation.loop.LoopExtraction;
import cx2x.translator.transformation.loop.LoopFusion;
import cx2x.translator.transformation.loop.LoopInterchange;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.xcodeml.transformation.*;

/**
 * ClawTransformer stores all transformation groups applied during the
 * translation.
 *
 * @author clementval
 */

public class ClawTransformer implements Transformer {
  private int _transformationCounter = 0;

  private TransformationGroup<LoopFusion> _loopFusion = null;
  private TransformationGroup<LoopInterchange> _loopInterchange = null;
  private TransformationGroup<LoopExtraction> _loopExtract = null;
  private TransformationGroup<UtilityRemove> _utilityRemove = null;
  private TransformationGroup<OpenAccContinuation> _openAccCont = null;
  private ArrayList<TransformationGroup> _transformationGroups = null;

  /**
   * ClawTransformer ctor. Creates the transformation groups needed for the CLAW
   * transformation and order the accordingly to their interpretation order.
   */
  public ClawTransformer(){
    _loopFusion = new DependentTransformationGroup<>("loop-fusion");
    _loopInterchange =
        new IndependentTransformationGroup<>("loop-interchange");
    _loopExtract =
        new IndependentTransformationGroup<>("loop-extract");
    _utilityRemove =
        new IndependentTransformationGroup<>("remove");
    _openAccCont =
        new IndependentTransformationGroup<>("open-acc-continuation");

    // Add transformations (order of insertion is the one that will be applied)
    _transformationGroups = new ArrayList<>();
    _transformationGroups.add(_utilityRemove);
    _transformationGroups.add(_loopExtract);
    _transformationGroups.add(_loopFusion);
    _transformationGroups.add(_loopInterchange);
    _transformationGroups.add(_openAccCont);
  }

  /**
   * @see Transformer#addTransformation(Transformation)
   */
  public void addTransformation(Transformation t){
    if(t instanceof LoopFusion){
      _loopFusion.add((LoopFusion)t);
    } else if(t instanceof LoopInterchange){
      _loopInterchange.add((LoopInterchange)t);
    } else if(t instanceof LoopExtraction){
      _loopExtract.add((LoopExtraction)t);
    } else if(t instanceof UtilityRemove){
      _utilityRemove.add((UtilityRemove)t);
    } else if (t instanceof OpenAccContinuation){
      _openAccCont.add((OpenAccContinuation)t);
    }
  }

  /**
   * @see Transformer#addTransformationGroup(TransformationGroup, int)
   */
  public void addTransformationGroup(TransformationGroup tg, int position){
    _transformationGroups.add(position, tg);
  }

  /**
   * @see Transformer#getGroups()
   */
  public List<TransformationGroup> getGroups(){
    return _transformationGroups;
  }

  /**
   * Get the next extraction counter value.
   * @return Transformation counter value.
   */
  public int getNextTransformationCounter(){
    return _transformationCounter++;
  }
}
