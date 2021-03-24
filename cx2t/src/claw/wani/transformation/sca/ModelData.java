/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import java.util.HashMap;
import java.util.Map;

import claw.shenron.transformation.Transformation;
import claw.shenron.translator.Translator;
import claw.tatsu.common.Utility;
import claw.tatsu.xcodeml.xnode.XnodeUtil;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.wani.language.ClawClause;
import claw.wani.language.ClawPragma;
import claw.wani.transformation.ClawBlockTransformation;
import claw.wani.x2t.translator.ClawTranslator;

/**
 * Model data directive analysis for the SCA transformation.
 *
 * @author clementval
 */
public class ModelData extends ClawBlockTransformation
{

    public ModelData(ClawPragma startDirective, ClawPragma endDirective)
    {
        super(startDirective, endDirective);
    }

    @Override
    public boolean analyze(XcodeProgram xcodeml, Translator translator)
    {
        /*
         * Discover variable part of the model configuration and the subroutine holding
         * them
         */

        ClawTranslator trans = (ClawTranslator) translator;

        if (!trans.cfg().getModelConfig().isLoaded())
        {
            xcodeml.addError("SCA model-data directive requires model configuration!", _clawStart.getPragma());
        }

        // Locate the subroutine/function in which the directive is defined
        FfunctionDefinition sub = getDirective().getPragma().findParentFunction();

        Map<String, String> modelVariables;
        if (trans.hasElement(sub) != null)
        {
            modelVariables = Utility.convertToMap(trans.hasElement(sub));
        } else
        {
            modelVariables = new HashMap<>();
        }

        for (String data : XnodeUtil.getAllVariables(getDirective().getPragma(), getEndDirective().getPragma()))
        {
            modelVariables.put(data, _clawStart.value(ClawClause.LAYOUT));
        }

        trans.storeElement(sub, modelVariables);

        return true;
    }

    @Override
    public boolean canBeTransformedWith(XcodeProgram xcodeml, Transformation other)
    {
        return false; // Independent transformation
    }

    @Override
    public void transform(XcodeProgram xcodeml, Translator translator, Transformation other)
    {
        // Analysis only transformation. All the work is done in SCA and SCAForward
        removePragma();
    }
}
