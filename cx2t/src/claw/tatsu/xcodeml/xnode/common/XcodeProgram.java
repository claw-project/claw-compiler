/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.xnode.common;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.w3c.dom.Document;

import claw.tatsu.common.Context;
import claw.tatsu.xcodeml.error.XanalysisError;
import claw.tatsu.xcodeml.xnode.Xname;
import claw.tatsu.xcodeml.xnode.XnodeUtil;

/*-
 * The XcodeProgram represents the XcodeProgram (2) element in XcodeML
 * intermediate representation.
 *
 * Elements: ( typeTable, globalSymbols, globalDeclarations )
 * - Required:
 * - typeTable (XtypeTable)
 * - globalSymbols (XsymbolTable)
 * - globalDeclarations (XglobalDeclTable)
 * Attributes:
 * - Optional: compiler-info (text), version (text), time (time),
 * language (text), source (text)
 *
 * @author clementval
 */

public class XcodeProgram extends XcodeML
{

    private final List<XanalysisError> _errors;
    private final List<XanalysisError> _warnings;
    // XcodeProgram inner elements
    private XsymbolTable _globalSymbolsTable = null;
    private XglobalDeclTable _globalDeclarationsTable = null;
    private Context _context;

    public Context context()
    {
        return _context;
    }

    /**
     * Default ctor used just to carry errors.
     */
    private XcodeProgram()
    {
        _errors = new ArrayList<>();
        _warnings = new ArrayList<>();
    }

    /**
     * XcodeProgram base constructor.
     *
     * @param doc The XcodeML document.
     */
    private XcodeProgram(Document doc, Context context)
    {
        super(doc);
        _errors = new ArrayList<>();
        _warnings = new ArrayList<>();
        _context = context;
    }

    public static XcodeProgram createFromStream(InputStream in, Context context)
    {
        BufferedInputStream bis = new BufferedInputStream(in);
        Document doc = readXmlStream(bis);
        return createFromDocument(doc, context);
    }

    /**
     * Create a XcodeProgram object from an XML document.
     *
     * @param doc Input DOM document.
     * @return A XcodeProgram object loaded with the information from the file. null
     *         if the file couldn't be read.
     */
    public static XcodeProgram createFromDocument(Document doc, Context context)
    {
        if (doc == null)
        {
            XcodeProgram program = new XcodeProgram();
            program.addError("Unable to read input XcodeML/F");
            return program;
        }
        XcodeProgram program = new XcodeProgram(doc, context);
        program.readDocumentInformation();
        if (!program.isXcodeMLvalid())
        {
            program.addError("XcodeML file is not valid");
        }
        return program;
    }

    /**
     * Create a XcodeProgram object from an XcodeML input file.
     *
     * @param input XcodeML input filename or path
     * @return An XcodeProgram object loaded with the information from the file.
     *         Null if the file couldn't be read.
     */
    public static XcodeProgram createFromFile(Path input, Context context)
    {
        Document doc = XnodeUtil.readXmlFile(input);
        return createFromDocument(doc, context);
    }

    /**
     * Read all the XcodeML document information: version, language, time, source,
     * compiler info.
     */
    private void readDocumentInformation()
    {
        readGlobalSymbolsTable();
        readGlobalDeclarationsTable();
    }

    /**
     * Add an error. If msg is null or empty, the error is not added.
     *
     * @param msg    Error message.
     * @param lineno Line number that triggered the error.
     */
    public void addError(String msg, int lineno)
    {
        addMsg(msg, lineno, _errors);
    }

    /**
     * Add an error. If msg is null or empty, the error is not added.
     *
     * @param msg  Error message.
     * @param node Node that triggered the error.
     */
    public void addError(String msg, Xnode node)
    {
        addMsg(msg, node != null ? node.lineNo() : 0, _errors);
    }

    /**
     * Add an error. If msg is null or empty, the error is not added.
     *
     * @param msg Error message.
     */
    public void addError(String msg)
    {
        addMsg(msg, _errors);
    }

    /**
     * Add an warning. If msg is null or empty, the warning is not added.
     *
     * @param msg Warning message.
     */
    public void addWarning(String msg)
    {
        addMsg(msg, 0, _warnings);
    }

    /**
     * Add an warning. If msg is null or empty, the warning is not added.
     *
     * @param msg Warning message.
     */
    public void addWarning(String msg, Xnode node)
    {
        addMsg(msg, node != null ? node.lineNo() : 0, _warnings);
    }

    /**
     * Add a new message to the error or the warning list
     *
     * @param msg    Message to be added.
     * @param lineno Line number that trigger the message.
     * @param list   List in which the message should be added.
     */
    private void addMsg(String msg, int lineno, List<XanalysisError> list)
    {
        addMsg(msg, Collections.singletonList(lineno), list);
    }

    /**
     * Add a new message to the error or the warning list
     *
     * @param msg    Message to be added.
     * @param lineno List of lines triggering the messages.
     * @param list   List in which the message should be added.
     */
    private void addMsg(String msg, List<Integer> lineno, List<XanalysisError> list)
    {
        if (msg == null || msg.isEmpty())
        {
            return;
        }
        list.add(new XanalysisError(msg, lineno));
    }

    /**
     * Add a new message to the error or the warning list
     *
     * @param msg  Message to be added.
     * @param list List in which the message should be added.
     */
    private void addMsg(String msg, List<XanalysisError> list)
    {
        addMsg(msg, 0, list);
    }

    /**
     * Check if the current translation unit has error.
     *
     * @return True if there is at least one error. False otherwise.
     */
    public boolean hasErrors()
    {
        return !_errors.isEmpty();
    }

    /**
     * Check if the current translation unit has warnings.
     *
     * @return True if there is at least one warning. False otherwise.
     */
    public boolean hasWarnings()
    {
        return !_warnings.isEmpty();
    }

    /**
     * Get all the errors.
     *
     * @return A list containing all the errors.
     */
    public List<XanalysisError> getErrors()
    {
        return _errors;
    }

    /**
     * Add a warning.
     *
     * @param msg    Warning message.
     * @param lineno Line number that triggered the warning.
     */
    public void addWarning(String msg, int lineno)
    {
        addMsg(msg, lineno, _warnings);
    }

    /**
     * Add a warning.
     *
     * @param msg    Warning message.
     * @param lineno Line numbers that triggered the warning.
     */
    public void addWarning(String msg, List<Integer> lineno)
    {
        addMsg(msg, lineno, _warnings);
    }

    /**
     * Get all the warnings.
     *
     * @return A list containing all the warnings.
     */
    public List<XanalysisError> getWarnings()
    {
        return _warnings;
    }

    /**
     * Get the symbols table of the XcodeML program.
     *
     * @return The symbols table.
     */
    public XsymbolTable getGlobalSymbolsTable()
    {
        return _globalSymbolsTable;
    }

    /**
     * Get the declarations table of the XcodeML program.
     *
     * @return The declarations table.
     */
    public XglobalDeclTable getGlobalDeclarationsTable()
    {
        return _globalDeclarationsTable;
    }

    /**
     * Check whether the XcodeML input file match the requirements.
     *
     * @return True if the XcodeML file matches the requirements.
     */
    private boolean isXcodeMLvalid()
    {
        if (getDocument() == null)
        {
            addError("Not an valid document");
            return false;
        }

        if (!is(Xcode.XCODE_PROGRAM))
        {
            addError("Not an XcodeProgram document");
            return false;
        }

        if (!Xname.SUPPORTED_VERSION.equals(getAttribute(Xattr.VERSION)))
        {
            addError("XcodeML version is not supported");
            return false;
        }

        if (!Xname.SUPPORTED_LANGUAGE.equals(getAttribute(Xattr.LANGUAGE)))
        {
            addError("Language is not set to Fortran");
            return false;
        }

        return true;
    }

    /**
     * Read the XcodeML global symbols table
     */
    private void readGlobalSymbolsTable()
    {
        _globalSymbolsTable = new XsymbolTable(matchSeq(Xcode.GLOBAL_SYMBOLS));
    }

    /**
     * Read the XcodeML global declarations table
     */
    private void readGlobalDeclarationsTable()
    {
        _globalDeclarationsTable = new XglobalDeclTable(matchSeq(Xcode.GLOBAL_DECLARATIONS));
    }

    /**
     * Get the XcodeML version.
     *
     * @return XcodeML version attribute value.
     */
    public String getVersion()
    {
        return getAttribute(Xattr.VERSION);
    }

    /**
     * Get the XcodeML language.
     *
     * @return XcodeML language attribute value.
     */
    public String getLanguage()
    {
        return getAttribute(Xattr.LANGUAGE);
    }

    /**
     * Get the XcodeML generation time.
     *
     * @return XcodeML time attribute value.
     */
    public String getTime()
    {
        return getAttribute(Xattr.TIME);
    }

    /**
     * Get the XcodeML source file information.
     *
     * @return Source file attribute value.
     */
    public String getSource()
    {
        return getAttribute(Xattr.SOURCE);
    }

    /**
     * Get the XcodeML source file information. Only filename without the directory.
     *
     * @return Filename part of the source file attribute value.
     */
    public String getSourceFileOnly()
    {
        String source = getSource();
        if (source == null)
        {
            return "";
        }
        return Paths.get(source).getFileName().toString();
    }

    /**
     * Get the XcodeML compiler information.
     *
     * @return Compiler information attribute value.
     */
    public String getCompilerInfo()
    {
        return getAttribute(Xattr.COMPILER_INFO);
    }
}
