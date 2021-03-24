/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package external.transformation;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.wani.transformation.ClawTransformation;

/**
 * Simple transformation that add print call before function call. This serves
 * as an example for external transformation.
 *
 * @author clementval
 */
public class AddPrint extends ClawTransformation
{

    /**
     * Constructs a new AddPrint transformation triggered for a translation unit.
     */
    public AddPrint()
    {
        super();
    }

    /**
     * No particular check.
     *
     * @param xcodeml    The XcodeML on which the transformations are applied.
     * @param translator The translator used to applied the transformations.
     * @return True the directive starts with the OpenACC prefix.
     */
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        return true;
    }

    /**
     * @return Always false as independent transformation are applied one by one.
     * @see Transformation#canBeTransformedWith(XcodeProgram, Transformation)
     */
    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        // independent transformation
        return false;
    }

    /**
     * Insert a print statement with the function name after each function calls.
     *
     * @param xcodeml        The XcodeML on which the transformations are applied.
     * @param translator     The translator used to applied the transformations.
     * @param transformation Not used in this transformation
     */
    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation transformation)
    {
        for (Xnode fctDef : xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION))
        {
            Xnode printStatement = xcodeml.createPrintStatement("*",
                    new String[] { "Call function", fctDef.matchDescendant(Xcode.NAME).value() });
            fctDef.body().insert(printStatement, false);
        }
    }
}
