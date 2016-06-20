/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.translator.transformer;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import cx2x.translator.common.GroupConfiguration;
import cx2x.translator.transformation.openacc.OpenAccContinuation;
import cx2x.xcodeml.transformation.*;
import cx2x.xcodeml.xnode.Xnode;
import org.w3c.dom.Element;

/**
 * ClawTransformer stores all transformation groups applied during the
 * translation.
 *
 * @author clementval
 */

public class ClawTransformer implements Transformer {
  private int _transformationCounter = 0;

  // Hold all transformation groups
  private final Map<Class, TransformationGroup> _tGroups;

  // Hold cross-transformation elements
  private final Map<Element, Object> _crossTransformationTable;


  /**
   * ClawTransformer ctor. Creates the transformation groups needed for the CLAW
   * transformation and order the accordingly to their interpretation order.
   * @param groups List of transformation groups that define the transformation
   *               order.
   */
  public ClawTransformer(List<GroupConfiguration> groups){
    /*
     * Use LinkedHashMap to be able to iterate through the map
     * entries with the insertion order.
     */
    _tGroups = new LinkedHashMap<>();
    for(GroupConfiguration g : groups){
      switch (g.getType()){
        case DEPENDENT:
          _tGroups.put(g.getTransformationClass(),
              new DependentTransformationGroup(g.getName()));
          break;
        case INDEPENDENT:
          _tGroups.put(g.getTransformationClass(),
              new IndependentTransformationGroup(g.getName()));
          break;
      }
    }

    // Internal transformations not specified by default configuration or user
    _tGroups.put(OpenAccContinuation.class,
        new IndependentTransformationGroup("internal-open-acc-continuation"));

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
  public Object hasElement(Xnode key){
    if(_crossTransformationTable.containsKey(key.getElement())){
      return _crossTransformationTable.get(key.getElement());
    }
    return null;
  }

  /**
   * Store a Xnode from a transformation for a possible usage in another
   * transformation. If a key is already present, the element is overwritten.
   * @param key   The element acting as a key.
   * @param value The element to be stored.
   */
  public void storeElement(Xnode key, Object value){
    if(_crossTransformationTable.containsKey(key.getElement())){
      _crossTransformationTable.remove(key.getElement());
    }
    _crossTransformationTable.put(key.getElement(), value);
  }
}
