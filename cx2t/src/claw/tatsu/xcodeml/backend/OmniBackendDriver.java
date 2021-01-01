/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.backend;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;

import xcodeml.util.IXmOption;
import xcodeml.util.XmDecompiler;
import xcodeml.util.XmDecompilerContext;
import xcodeml.util.XmException;
import xcodeml.util.XmToolFactory;

/**
 * Wrapper class to call the Fortran decompiler of OMNI Compiler directly from
 * Java instead of calling it as a separated program.
 *
 * @author clementval
 */
public class OmniBackendDriver
{
    private XmToolFactory _toolFactory;

    /**
     * Constructs a new OmniBackendDriver object.
     *
     * @param lang Language of output.
     * @throws XmException If instantiation of the XmToolFactory fails.
     */
    public OmniBackendDriver(Lang lang) throws XmException
    {
        if (lang == Lang.FORTRAN)
        {
            _toolFactory = new XmToolFactory("F");
        } else if (lang == Lang.C)
        {
            _toolFactory = new XmToolFactory("C");
        }
    }

    /**
     * Decompile the XcodeML file into Fortran code.
     *
     * @param outputFilepath Fortran output file path.
     * @param xcodeml        XcodeML document.
     * @param maxColumns     Maximum number of column for the output file.
     * @param lineDirectives If true, preprocessor line directives are added.
     * @throws XmException
     */
    public void decompile(OutputStream output, Document xcodeml, int maxColumns, boolean lineDirectives,
            IXmOption xmOption) throws XmException
    {
        if (!lineDirectives)
        {
            xmOption.setIsSuppressLineDirective(true);
        }
        xmOption.setDebugOutput(false);

        PrintWriter writer = new PrintWriter(output);
        XmDecompiler decompiler = _toolFactory.createDecompiler();
        XmDecompilerContext context = _toolFactory.createDecompilerContext(xmOption);

        if (maxColumns > 0)
        {
            context.setProperty(XmDecompilerContext.KEY_MAX_COLUMNS, "" + maxColumns);
        }

        decompiler.decompile(context, xcodeml, writer);
        writer.flush();
    }

    /**
     * Decompile the XcodeML file into Fortran code.
     *
     * @param inputFilepath  XcodeML input file path.
     * @param maxColumns     Maximum number of column for the output file.
     * @param lineDirectives If true, preprocessor line directives are added.
     * @return True if the decompilation succeeded. False otherwise.
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     * @throws XmException
     */
    public void decompileFromFile(OutputStream output, Path inputFilePath, int maxColumns, boolean lineDirectives,
            IXmOption xmOption) throws IOException, ParserConfigurationException, SAXException, XmException
    {
        Document xcodeml;
        try (InputStream in = Files.newInputStream(inputFilePath))
        {
            DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = docFactory.newDocumentBuilder();
            xcodeml = builder.parse(in);
        }
        decompile(output, xcodeml, maxColumns, lineDirectives, xmOption);
    }

    public enum Lang {
        C, FORTRAN
    }
}
