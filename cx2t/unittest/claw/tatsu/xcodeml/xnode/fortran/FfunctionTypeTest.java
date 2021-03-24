/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.fortran;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import helper.Utils.TestContext;
import helper.XmlHelper;

/**
 * Test FfunctionType features
 *
 * @author clementval
 */
public class FfunctionTypeTest
{

    private static final String fctType1 = "<FfunctionType type=\"F0\" "
            + "return_type=\"Freal\"><params><name type=\"Fint\">a</name>"
            + "<name type=\"Fint\">b</name></params></FfunctionType>";

    /**
     * Test simple fct type
     *
     * function foo(a,b) integer a, b
     */
    @Test
    public void simpleFctTypeTest()
    {
        Context context = new TestContext();
        FfunctionType f = XmlHelper.createXfctTypeFromString(fctType1);
        assertFunctionType(f);

        FfunctionType clone = f.cloneNode();
        assertNotEquals(clone.element(), f.element());
        assertFunctionType(clone);

        XcodeProgram xcodeml = XmlHelper.getDummyXcodeProgram(context);

        FfunctionType emptyFctType = xcodeml.createFunctionType(null);
        assertFalse(emptyFctType.hasParam("a"));

        Xnode paramA = xcodeml.createName("a", FortranType.INTEGER.toString());
        Xnode paramB = xcodeml.createName("b", FortranType.INTEGER.toString());
        Xnode paramC = xcodeml.createName("c", FortranType.INTEGER.toString());
        Xnode paramD = xcodeml.createName("d", FortranType.INTEGER.toString());
        emptyFctType.addParameters(paramB);
        assertEquals(1, emptyFctType.getParameters().size());
        emptyFctType.addParameters(paramB, paramA);
        assertEquals(2, emptyFctType.getParameters().size());
        emptyFctType.addParameters(paramD, paramC);
        assertEquals(3, emptyFctType.getParameters().size());

        emptyFctType.addParameters(null);
        assertEquals(3, emptyFctType.getParameters().size());

        emptyFctType.addParameters(null, null);
        assertEquals(3, emptyFctType.getParameters().size());

        assertEquals("a", emptyFctType.getParameters().get(0).value());
        assertEquals("b", emptyFctType.getParameters().get(1).value());
        assertEquals("c", emptyFctType.getParameters().get(2).value());

        emptyFctType.addParameters(null, paramD);
        assertEquals(4, emptyFctType.getParameters().size());
        assertEquals("d", emptyFctType.getParameters().get(3).value());

        List<String> names = emptyFctType.getParamsNames();
        assertEquals(4, names.size());
        assertEquals("a", names.get(0));
        assertEquals("b", names.get(1));
        assertEquals("c", names.get(2));
        assertEquals("d", names.get(3));
    }

    private void assertFunctionType(FfunctionType f)
    {
        assertNotNull(f);

        assertEquals("Freal", f.getReturnType());
        assertNull(f.getResultName());
        assertFalse(f.isRecursive());
        assertFalse(f.isProgram());
        assertFalse(f.isInternal());
        assertFalse(f.isPure());
        assertFalse(f.isElemental());
        assertEquals("F0", f.getType());

        // Test parameters
        assertEquals(2, f.getParameters().size());
        assertEquals("a", f.getParameters().get(0).value());
        assertEquals("Fint", f.getParameters().get(0).getType());
        assertEquals("b", f.getParameters().get(1).value());
        assertEquals("Fint", f.getParameters().get(1).getType());

        List<String> names = f.getParamsNames();
        List<String> expected = Arrays.asList("a", "b");
        assertEquals(expected.size(), names.size());
        for (int i = 0; i < names.size(); ++i)
        {
            assertEquals(expected.get(i), names.get(i));
        }
        assertTrue(f.hasParam("a"));
        assertFalse(f.hasParam("z"));

        assertNotNull(f.toString());
    }
}
