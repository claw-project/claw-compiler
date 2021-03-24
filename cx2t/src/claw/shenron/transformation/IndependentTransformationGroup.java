/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.transformation;

import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;

/**
 * An independent transformation group applies each transformation without
 * checking with any other transformation in the pipeline.
 *
 * @author clementval
 */

public class IndependentTransformationGroup extends TransformationGroup
{

    /**
     * IndependentTransformationGroup ctor
     *
     * @param name A friendly name to describe the transformation group.
     */
    public IndependentTransformationGroup(String name)
    {
        super(name);
    }

    /**
     * @see TransformationGroup#applyTransformations(XcodeProgram, Translator)
     */
    public void applyTransformations(XcodeProgram xcodeml, Translator translator) throws Exception
    {
        for (Transformation trans : getTransformations())
        {
            try
            {
                trans.transform(xcodeml, translator, null);
                if (trans.isTransformed())
                {
                    incrementAppliedTransformation();
                }
            } catch (IllegalTransformationException itex)
            {
                // Catch the exception to add line information and rethrow it
                if (itex.getStartLine() == 0)
                {
                    itex.setStartLine(trans.getStartLine());
                }
                throw itex;
            }
        }
    }
}
