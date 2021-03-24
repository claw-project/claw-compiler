/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.tatsu.xcodeml.error;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * This class hold information about error happening during transformation
 * analysis.
 *
 * @author clementval
 */

public class XanalysisError
{

    private final String _errorMsg;
    private final List<Integer> _errorLineNumbers;

    /**
     * Default ctor.
     *
     * @param msg    Error message
     * @param lineno Line number that triggered the error.
     */
    public XanalysisError(String msg, int lineno)
    {
        _errorMsg = msg;
        _errorLineNumbers = new ArrayList<>();
        _errorLineNumbers.add(lineno);
    }

    /**
     * Default ctor with more than one line number.
     *
     * @param msg    Error message
     * @param lineno Line numbers that triggered the error.
     */
    public XanalysisError(String msg, List<Integer> lineno)
    {
        _errorMsg = msg;
        if (lineno == null)
        {
            _errorLineNumbers = new ArrayList<>();
        } else
        {
            _errorLineNumbers = lineno;
        }
    }

    /**
     * @return The error message
     */
    public String getMessage()
    {
        return _errorMsg;
    }

    /**
     * @return The first line number that triggered the error.
     */
    public int getLine()
    {
        return _errorLineNumbers.get(0);
    }

    /**
     * @return The line numbers that triggered the error.
     */
    public List<Integer> getLines()
    {
        return _errorLineNumbers;
    }

    /**
     * String concatenation of the lines present in the error/warning.
     *
     * @return String value of the line numbers.
     */
    public String getConcatLines()
    {
        return _errorLineNumbers.stream().map(String::valueOf).collect(Collectors.joining(","));
    }
}
