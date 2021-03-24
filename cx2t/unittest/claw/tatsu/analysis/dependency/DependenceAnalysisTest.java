/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.analysis.dependency;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.CompilerDirective;
import claw.tatsu.common.Context;
import claw.tatsu.common.Target;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test the various features of the DependenceAnalysis class and the
 * IterationSpace class.
 *
 * @author clementval
 */
public class DependenceAnalysisTest
{

    /**
     * Test the analysis feature of the DependenceAnalysis class.
     */
    @Test
    public void analyzeTest()
    {
        Context context = new TestContext();
        // Load test data file
        assertTrue(Files.exists(TestConstant.TEST_DEPENDENCE));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DEPENDENCE, context);
        assertNotNull(xcodeml);

        // Match all the function definitions
        List<Xnode> functions = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(2, functions.size());

        // Match all the pragma
        List<Xnode> pragmas = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
        assertEquals(1, pragmas.size());

        // Get the function definition that interests us
        Xnode fctDef = functions.get(0);

        List<Xnode> loops = fctDef.matchAll(Xcode.F_DO_STATEMENT);
        assertEquals(10, loops.size());

        // Create dependence analysis object for each do statement
        List<DependenceAnalysis> dependencies = new ArrayList<>();
        for (Xnode loop : loops)
        {
            try
            {
                dependencies.add(new DependenceAnalysis(loop));
            } catch (Exception e)
            {
                fail();
            }
        }

        // Assert the information for each do statement
        assertTrue(dependencies.get(0).isIndependent());
        assertTrue(dependencies.get(1).isIndependent());
        assertTrue(dependencies.get(2).isIndependent());

        assertFalse(dependencies.get(3).isIndependent());
        assertEquals(1, dependencies.get(3).getDistanceVector());
        assertEquals(DependenceDirection.BACKWARD, dependencies.get(3).getDirectionVector());

        assertFalse(dependencies.get(4).isIndependent());
        assertEquals(1, dependencies.get(4).getDistanceVector());
        assertEquals(DependenceDirection.FORWARD, dependencies.get(4).getDirectionVector());

        assertFalse(dependencies.get(5).isIndependent());
        assertEquals(1, dependencies.get(5).getDistanceVector());
        assertEquals(DependenceDirection.FORWARD, dependencies.get(5).getDirectionVector());

        assertFalse(dependencies.get(6).isIndependent());
        assertEquals(1, dependencies.get(6).getDistanceVector());
        assertEquals(DependenceDirection.BACKWARD, dependencies.get(6).getDirectionVector());

        assertTrue(dependencies.get(7).isIndependent());
        assertTrue(dependencies.get(8).isIndependent());
        assertTrue(dependencies.get(9).isIndependent());
    }

    /**
     * Test the IterationSpace feature of fusion and check the results.
     */
    @Test
    public void analyzeTest3d()
    {
        Context context = new TestContext();
        // Load test data file
        assertTrue(Files.exists(TestConstant.TEST_DEPENDENCE_3D));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DEPENDENCE_3D, context);
        assertNotNull(xcodeml);

        // Match all the function definitions
        List<Xnode> functions = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(2, functions.size());

        // Match all the pragmas
        List<Xnode> pragmas = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
        assertEquals(1, pragmas.size());

        // Analyze the pragma
        context.init(CompilerDirective.OPENACC, Target.GPU, null, 80);

        // Get the function definition that interests us
        Xnode fctDef = functions.get(0);

        // Match all the do statements in the function
        List<Xnode> loops = fctDef.matchAll(Xcode.F_DO_STATEMENT);
        assertEquals(11, loops.size());
    }

    @Test
    public void perfectlyNestedNoDependencyTest()
    {
        Context context = new TestContext();
        // Load test data file
        assertTrue(Files.exists(TestConstant.TEST_PERFECTLY_NESTED_NO_DEP));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_PERFECTLY_NESTED_NO_DEP, context);
        assertNotNull(xcodeml);

        // Match all the function definitions
        List<Xnode> functions = xcodeml.matchAll(Xcode.F_FUNCTION_DEFINITION);
        assertEquals(1, functions.size());

        // Match all the pragmas
        List<Xnode> pragmas = xcodeml.matchAll(Xcode.F_PRAGMA_STATEMENT);
        assertEquals(1, pragmas.size());

        // Analyze the pragma
        context.init(CompilerDirective.OPENACC, Target.GPU, null, 80);

        // Get the function definition that interests us
        Xnode fctDef = functions.get(0);

        // Match all the do statements in the function
        List<Xnode> loops = fctDef.matchAll(Xcode.F_DO_STATEMENT);
        assertEquals(2, loops.size());

        // Create an iteration space
        try
        {
            IterationSpace is = new IterationSpace(loops);
            System.out.println();
            assertEquals(2, is.getNbLevel());
            is.printDebug(true);
            assertTrue(is.isPerfectlyNested());
        } catch (Exception e)
        {
            fail();
        }
    }
}
