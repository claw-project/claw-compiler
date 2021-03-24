/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.transformation;

import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;

import java.util.ArrayList;
import java.util.List;

/**
 * A TransformationGroup holds transformation units and can apply. Only derived
 * classes of TransformationGroup must be used as they implement
 * applyTransformations.
 *
 * @author clementval
 */

public abstract class TransformationGroup
{

    private final String _name;
    private List<Transformation> _transformations;
    private int _appliedTransformation;

    /**
     * TransformationGroup ctor.
     *
     * @param name A friendly name to describe the transformation group.
     */
    public TransformationGroup(String name)
    {
        _name = name;
        _transformations = new ArrayList<>();
    }

    /**
     * Get the number of transformation stored in the group.
     *
     * @return The number of transformation in the group.
     */
    public int count()
    {
        return _transformations.size();
    }

    /**
     * Add a new transformation in the group.
     *
     * @param transformation The transformation to be added.
     */
    public void add(Transformation transformation)
    {
        if (transformation != null)
        {
            _transformations.add(transformation);
        }
    }

    /**
     * Return the list of all transformations in this group.
     *
     * @return A list of Transformation object.
     */
    public List<Transformation> getTransformations()
    {
        return _transformations;
    }

    /**
     * Set the list of transformations.
     *
     * @param transformations New list of transformations.
     */
    public void setTransformations(List<Transformation> transformations)
    {
        _transformations = transformations;
    }

    /**
     * Get the name of transformation stored in this group.
     *
     * @return A string representing the name of the transformations.
     */
    public String transformationName()
    {
        return _name;
    }

    /**
     * Return the number of transformation actually applied in the group.
     *
     * @return Number of applied transformation.
     */
    public int getAppliedTransformationCount()
    {
        return _appliedTransformation;
    }

    /**
     * Increment the applied transformation counter.
     */
    public void incrementAppliedTransformation()
    {
        ++_appliedTransformation;
    }

    /**
     * Apply all transformation stored in this group. Method transform from each
     * transformation is called.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @throws IllegalTransformationException if transformation cannot be applied.
     * @see Transformation#transform(XcodeProgram, Translator, Transformation)
     */
    public abstract void applyTransformations(XcodeProgram xcodeml, Translator translator) throws Exception;
}
