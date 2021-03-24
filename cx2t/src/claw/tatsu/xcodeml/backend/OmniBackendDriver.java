/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.backend;

import claw.tatsu.xcodeml.xnode.common.XcodeML;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;
import xcodeml.util.*;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;

/**
 * Wrapper class to call the Fortran decompiler of OMNI Compiler directly from
 * Java instead of calling it as a separated program.
 *
 * @author clementval
 */
public class OmniBackendDriver
{

    private BufferedReader _reader;
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

    private boolean openXcodeMLFile(String inputFilepath)
    {
        if (_reader != null)
        {
            try
            {
                _reader.close();
            } catch (IOException e)
            {
                System.err.println(e.getMessage());
                return false;
            }
        }
        try
        {
            _reader = new BufferedReader(new FileReader(inputFilepath));
            return true;
        } catch (IOException e)
        {
            return false;
        }
    }

    /**
     * Decompile the XcodeML file into Fortran code.
     *
     * @param outputFilepath Fortran output file path.
     * @param xcodeml        XcodeML translation unit.
     * @param maxColumns     Maximum number of column for the output file.
     * @param lineDirectives If true, preprocessor line directives are added.
     * @return True if the decompilation succeeded. False otherwise.
     */
    public boolean decompile(String outputFilepath, XcodeML xcodeml, int maxColumns, boolean lineDirectives)
    {
        return decompile(outputFilepath, xcodeml.getDocument(), maxColumns, lineDirectives);
    }

    /**
     * Decompile the XcodeML file into Fortran code.
     *
     * @param outputFilepath Fortran output file path.
     * @param xcodeml        XcodeML document.
     * @param maxColumns     Maximum number of column for the output file.
     * @param lineDirectives If true, preprocessor line directives are added.
     * @return True if the decompilation succeeded. False otherwise.
     */
    private boolean decompile(String outputFilepath, Document xcodeml, int maxColumns, boolean lineDirectives)
    {
        if (!lineDirectives)
        {
            XmOption.setIsSuppressLineDirective(true);
        }
        XmOption.setDebugOutput(false);

        PrintWriter writer = null;
        try
        {
            if (outputFilepath == null || outputFilepath.isEmpty())
            {
                writer = new PrintWriter(System.out);
            } else
            {
                writer = new PrintWriter(new BufferedWriter(new FileWriter(outputFilepath)));
            }
        } catch (IOException e)
        {
            System.err.println(e.getMessage());
        }

        try
        {
            XmDecompiler decompiler = _toolFactory.createDecompiler();
            XmDecompilerContext context = _toolFactory.createDecompilerContext();

            if (maxColumns > 0)
            {
                context.setProperty(XmDecompilerContext.KEY_MAX_COLUMNS, "" + maxColumns);
            }

            decompiler.decompile(context, xcodeml, writer);

            if (writer != null)
            {
                writer.flush();
            } else
            {
                return false;
            }
            return true;
        } catch (Exception ex)
        {
            if (_reader != null)
            {
                try
                {
                    _reader.close();
                } catch (IOException ignored)
                {
                }
            }

            if (writer != null)
            {
                writer.close();
            }
        }
        return false;
    }

    /**
     * Decompile the XcodeML file into Fortran code.
     *
     * @param outputFilepath Fortran output file path.
     * @param inputFilepath  XcodeML input file path.
     * @param maxColumns     Maximum number of column for the output file.
     * @param lineDirectives If true, preprocessor line directives are added.
     * @return True if the decompilation succeeded. False otherwise.
     */
    public boolean decompileFromFile(String outputFilepath, String inputFilepath, int maxColumns,
            boolean lineDirectives)
    {
        if (!openXcodeMLFile(inputFilepath))
        {
            return false;
        }

        try
        {
            DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = docFactory.newDocumentBuilder();
            Document xcodeml = builder.parse(inputFilepath);
            return decompile(outputFilepath, xcodeml, maxColumns, lineDirectives);
        } catch (ParserConfigurationException | SAXException | IOException e)
        {
            return false;
        }
    }

    public enum Lang {
        C, FORTRAN
    }
}
