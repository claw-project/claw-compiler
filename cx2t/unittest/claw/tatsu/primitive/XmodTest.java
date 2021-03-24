/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.primitive;

import static junit.framework.TestCase.assertEquals;

import org.junit.Test;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import helper.Utils.TestContext;

/**
 * Test methods of the Xmod class.
 *
 * @author clementval
 */
public class XmodTest
{
    @Test
    public void getSuffixTest()
    {
        Context context = new TestContext();
        context.init(CompilerDirective.OPENACC, Target.GPU, null, 80);
        // .[directive].[target].claw
        assertEquals(".openacc.gpu.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.OPENMP, Target.CPU, null, 80);
        assertEquals(".openmp.cpu.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.NONE, Target.CPU, null, 80);
        assertEquals(".none.cpu.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.OPENMP, Target.MIC, null, 80);
        assertEquals(".openmp.mic.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.NONE, Target.FPGA, null, 80);
        assertEquals(".none.fpga.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.OPENACC, null, null, 80);
        assertEquals(".openacc.none.claw.xmod", Xmod.getSuffix(context));
        context.init(null, null, null, 80);
        assertEquals(".none.none.claw.xmod", Xmod.getSuffix(context));
        context.init(CompilerDirective.NONE, Target.GPU, null, 80);
        assertEquals(".none.gpu.claw.xmod", Xmod.getSuffix(context));
    }
}
