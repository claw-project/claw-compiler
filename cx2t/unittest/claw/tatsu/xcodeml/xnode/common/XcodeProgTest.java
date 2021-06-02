/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collections;

import org.junit.Test;

import claw.tatsu.common.Context;
import helper.TestConstant;
import helper.Utils.TestContext;

/**
 * Test the features of the XcodeProgram class
 *
 * @author clementval
 */

public class XcodeProgTest
{

    @Test
    public void basicXcodeProgTest()
    {
        Context context = new TestContext();
        assertTrue(Files.exists(TestConstant.TEST_DATA));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DATA, context);
        assertNotNull(xcodeml);
        assertNotNull(xcodeml.getTime());
        assertNotNull(xcodeml.getCompilerInfo());
        assertNotNull(xcodeml.getVersion());
        assertNotNull(xcodeml.getLanguage());
        assertNotNull(xcodeml.getSource());
        assertNotNull(xcodeml.getSourceFileOnly());
        assertEquals("original_code.f90", xcodeml.getSourceFileOnly());
        assertEquals(8, xcodeml.getTypeTable().size());
        assertEquals(2, xcodeml.getGlobalSymbolsTable().size());
        assertEquals(2, xcodeml.getGlobalDeclarationsTable().size());

        // Try to add not meaningful messages in errors
        xcodeml.addError("");
        xcodeml.addError("", 0);
        xcodeml.addError(null, 0);
        assertFalse(xcodeml.hasErrors());
        assertTrue(xcodeml.getErrors().isEmpty());

        xcodeml.addError("Error1", 1);
        assertTrue(xcodeml.hasErrors());
        assertEquals(1, xcodeml.getErrors().size());

        // Try to add not meaningful messages in warnings
        xcodeml.addWarning("");
        xcodeml.addWarning(null, 0);
        xcodeml.addWarning("", 0);
        xcodeml.addWarning(null, Collections.emptyList());
        assertFalse(xcodeml.hasWarnings());
        assertTrue(xcodeml.getWarnings().isEmpty());

        xcodeml.addWarning("New warning 1", 1);
        assertTrue(xcodeml.hasWarnings());
        assertEquals(1, xcodeml.getWarnings().size());
    }

    @Test
    public void createUnvalidXcodeProgram() throws IOException
    {
        Context context = new TestContext();

        final String d1 = "<XcodeProgram source=\"original_code.f90\"\n" + "language=\"Fortran\"\n"
                + "time=\"2015-11-25 14:47:59\"\n" + "compiler-info=\"XcodeML/Fortran-FrontEnd\"\n"
                + "version=\"1.5\"></XcodeProgram>";
        final String d2 = "<XcodeProgram source=\"original_code.f90\"\n" + "language=\"C\"\n"
                + "time=\"2015-11-25 14:47:59\"\n" + "compiler-info=\"XcodeML/Fortran-FrontEnd\"\n"
                + "version=\"1.0\"></XcodeProgram>";

        // Simulate STDIN
        try (ByteArrayInputStream in = new ByteArrayInputStream(d1.getBytes()))
        {
            // Version not valid
            XcodeProgram xc1 = XcodeProgram.createFromStream(in, context);
            assertNotNull(xc1);
            assertTrue(xc1.hasErrors());
        }

        try (ByteArrayInputStream in = new ByteArrayInputStream(d2.getBytes()))
        {
            // Language not valid
            XcodeProgram xc2 = XcodeProgram.createFromStream(in, context);
            assertNotNull(xc2);
            assertTrue(xc2.hasErrors());
        }

    }

    @Test
    public void creationFailingTest() throws IOException
    {
        Context context = new TestContext();
        XcodeProgram xc1 = XcodeProgram.createFromDocument(null, context);
        assertNotNull(xc1);
        assertTrue(xc1.hasErrors());

        // Simulate STDIN
        try (ByteArrayInputStream in = new ByteArrayInputStream("<dummy></dummy>".getBytes()))
        {
            XcodeProgram xc2 = XcodeProgram.createFromStream(in, context);
            assertNotNull(xc2);
            assertTrue(xc2.hasErrors());
        }
    }
}
