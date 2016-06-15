/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */

package cx2x.xcodeml.transformation;

import cx2x.xcodeml.xnode.XcodeProgram;
import cx2x.xcodeml.exception.*;
import java.util.ArrayList;
import java.util.List;

/**
 * A TransformationGroup holds transformation units and can apply. Only derived
 * classes of TransformationGroup must be used as they implement
 * applyTranslations.
 *
 * @author clementval
 */

public abstract class TransformationGroup {
  private final String _name;
  private final List<Transformation> _transformations;

  /**
   * TransformationGroup ctor.
   * @param name A friendly name to describe the transformation group.
   */
  public TransformationGroup(String name){
    _name = name;
    _transformations = new ArrayList<>();
  }

  /**
   * Get the number of transformation stored in the group.
   * @return The number of transformation in the group.
   */
  public int count(){
    return _transformations.size();
  }

  /**
   * Add a new transformation in the group.
   * @param translation The transformation to be added.
   */
  public void add(Transformation translation){
    _transformations.add(translation);
  }

  /**
   * Return the list of all transformations in this group.
   * @return A list of Transformation object.
   */
  protected List<Transformation> getTransformations(){
    return _transformations;
  }

  /**
   * Get the name of transformation stored in this group.
   * @return A string representing the name of the transformations.
   */
  public String transformationName(){
    return _name;
  }

  /**
   * Apply all transformation stored in this group. Method transform from each
   * transformation is called.
   * @see Transformation#transform(XcodeProgram, Transformer, Transformation)
   * @param xcodeml     The XcodeML on which the transformations are applied.
   * @param transformer The transformer used to applied the transformations.
   * @throws IllegalTransformationException if transformation cannot be applied.
   */
  public abstract void applyTranslations(XcodeProgram xcodeml,
    Transformer transformer) throws Exception;

}
