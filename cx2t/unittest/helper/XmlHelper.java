/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package helper;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.StringReader;
import java.nio.file.Files;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.common.XcodeProgram;
import claw.tatsu.xcodeml.xnode.common.XdeclTable;
import claw.tatsu.xcodeml.xnode.common.XglobalDeclTable;
import claw.tatsu.xcodeml.xnode.common.Xid;
import claw.tatsu.xcodeml.xnode.common.Xnode;
import claw.tatsu.xcodeml.xnode.common.XsymbolTable;
import claw.tatsu.xcodeml.xnode.common.XtypeTable;
import claw.tatsu.xcodeml.xnode.fortran.FbasicType;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionDefinition;
import claw.tatsu.xcodeml.xnode.fortran.FfunctionType;

/**
 * Helper class containing static methods for the unit tests.
 *
 * @author clementval
 */

public class XmlHelper
{

    private XmlHelper()
    {
        // Hide implicit public ctor
    }

    public static XcodeProgram getDummyXcodeProgram(Context context)
    {
        assertTrue(Files.exists(TestConstant.TEST_DATA));
        XcodeProgram xcodeml = XcodeProgram.createFromFile(TestConstant.TEST_DATA, context);
        assertNotNull(xcodeml);
        return xcodeml;
    }

    private static Document loadXMLFromString(String xml)
    {
        try
        {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            InputSource is = new InputSource(new StringReader(xml));
            return builder.parse(is);
        } catch (Exception ex)
        {
            return null;
        }
    }

    private static Xnode getElementFromString(String xml)
    {
        Document doc = loadXMLFromString(xml);
        if (doc != null)
        {
            return new Xnode(doc.getDocumentElement());
        }
        return null;
    }

    public static Xid createXidFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new Xid(n);
    }

    public static FbasicType createXbasicTypeFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new FbasicType(n);
    }

    public static FfunctionType createXfctTypeFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new FfunctionType(n);
    }

    public static XsymbolTable createXSymbolTableFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new XsymbolTable(n);
    }

    public static XtypeTable createXtypeTableFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new XtypeTable(n);
    }

    public static FfunctionDefinition createXfunctionDefinitionFromString(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new FfunctionDefinition(n);
    }

    public static XglobalDeclTable createGlobalDeclTable(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new XglobalDeclTable(n);
    }

    public static XdeclTable createXdeclTable(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return new XdeclTable(n);
    }

    public static Xnode createXpragma()
    {
        String xml = "<" + Xname.F_PRAGMA_STMT + "></" + Xname.F_PRAGMA_STMT + ">";
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return n;
    }

    public static Xnode createXnode(String xml)
    {
        Xnode n = XmlHelper.getElementFromString(xml);
        assertNotNull(n);
        return n;
    }
}
