/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.shenron.translator;

import java.util.Map;

import claw.shenron.transformation.Transformation;
import claw.shenron.transformation.TransformationGroup;
import claw.tatsu.xcodeml.exception.IllegalDirectiveException;
import claw.tatsu.xcodeml.exception.IllegalTransformationException;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;

/**
 * Translator interface
 *
 * Translator stores all the transformation to be applied by a translator.
 *
 * @author clementval
 */

public interface Translator
{

    /**
     * Generate transformation according to the pragma.
     *
     * @param xcodeml Current XcodeML unit.
     * @param pragma  Pragma that can trigger a transformation.
     * @throws IllegalDirectiveException      If directive is not formatted
     *                                        correctly.
     * @throws IllegalTransformationException If transformation cannot be generated.
     */
    void generateTransformation(XcodeProgram xcodeml, Xnode pragma)
            throws IllegalDirectiveException, IllegalTransformationException;

    /**
     * Add a transformation to the translator.
     *
     * @param xcodeml Current translation unit.
     * @param t       Transformation to add.
     * @throws IllegalTransformationException If transformation cannot be added.
     */
    void addTransformation(XcodeProgram xcodeml, Transformation t) throws IllegalTransformationException;

    /**
     * Check if the given pragma can be handled by the current translator.
     *
     * @param pragma Pragma statement node.
     * @return True if the translator can handle the pragma. False otherwise.
     */
    boolean isHandledPragma(Xnode pragma);

    /**
     * Perform last tasks before applying transformations.
     *
     * @param xcodeml Current XcodeML unit.
     * @throws IllegalTransformationException If translation cannot be finalized.
     */
    void finalizeTranslation(XcodeProgram xcodeml) throws IllegalTransformationException;

    /**
     * Get all transformation groups stored in this translator.
     *
     * @return A list of all transformation groups.
     */
    Map<Class<?>, TransformationGroup> getGroups();

    /**
     * Get the next transformation counter value.
     *
     * @return Transformation counter value.
     */
    int getNextTransformationCounter();

}
