/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformer;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import cx2x.translator.transformation.claw.ArrayToFctCall;
import cx2x.translator.transformation.claw.Kcaching;
import cx2x.translator.transformation.loop.*;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.translator.transformation.utility.UtilityRemove;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.xelement.XbaseElement;
import org.w3c.dom.Element;

/**
 * ClawTransformer stores all transformation groups applied during the
 * translation.
 *
 * @author clementval
 */

public class ClawTransformer implements Transformer {
  private int _transformationCounter = 0;

  // Hold all tranformation groups
  private final Map<Class, TransformationGroup> _tGroups;

  // Hold cross-transformation elements
  private final Map<Element, XbaseElement> _crossTransformationTable;


  /**
   * ClawTransformer ctor. Creates the transformation groups needed for the CLAW
   * transformation and order the accordingly to their interpretation order.
   */
  public ClawTransformer(){
    /*
     * Use LinkedHashMap to be able to iterate through the map
     * entries with the insertion order.
     */
    // TODO read configuration from a config file for transformation order
    _tGroups = new LinkedHashMap<>();
    _tGroups.put(UtilityRemove.class,
        new IndependentTransformationGroup("remove"));
    _tGroups.put(ArrayTransform.class,
        new IndependentTransformationGroup("array-transform"));
    _tGroups.put(LoopExtraction.class,
        new IndependentTransformationGroup("loop-extract"));
    _tGroups.put(LoopFusion.class,
        new DependentTransformationGroup("loop-fusion"));
    _tGroups.put(LoopHoist.class,
        new IndependentTransformationGroup("loop-hoist"));
    _tGroups.put(LoopInterchange.class,
        new IndependentTransformationGroup("loop-interchange"));
    _tGroups.put(ArrayToFctCall.class,
        new IndependentTransformationGroup("call"));
    _tGroups.put(Kcaching.class,
        new IndependentTransformationGroup("kcache"));
    _tGroups.put(OpenAccContinuation.class,
        new IndependentTransformationGroup("open-acc-continuation"));

    _crossTransformationTable = new HashMap<>();
  }

  /**
   * @see Transformer#addTransformation(Transformation)
   */
  public void addTransformation(Transformation t){
    if(_tGroups.containsKey(t.getClass())){
      _tGroups.get(t.getClass()).add(t);
    }
  }

  /**
   * @see Transformer#getGroups()
   */
  public Map<Class, TransformationGroup> getGroups(){
    return _tGroups;
  }

  /**
   * Get the next extraction counter value.
   * @return Transformation counter value.
   */
  public int getNextTransformationCounter(){
    return _transformationCounter++;
  }


  /**
   * Get a stored element from a previous transformation.
   * @param key Key to use to retrieve the element.
   * @return The stored element if present. Null otherwise.
   */
  public XbaseElement hasElement(XbaseElement key){
    if(_crossTransformationTable.containsKey(key.getBaseElement())){
      return _crossTransformationTable.get(key.getBaseElement());
    }
    return null;
  }

  /**
   * Store a XbaseElement from a transformation for a possible usage in another
   * transformation. If a key is already present, the element is overwritten.
   * @param key   The element acting as a key.
   * @param value The element to be stored.
   */
  public void storeElement(XbaseElement key, XbaseElement value){
    if(_crossTransformationTable.containsKey(key.getBaseElement())){
      _crossTransformationTable.remove(key.getBaseElement());
    }
    _crossTransformationTable.put(key.getBaseElement(), value);
  }
}
