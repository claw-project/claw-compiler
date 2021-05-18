/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

/**
 * <code>FortranException</code> encapsulates information about the error which
 * occurred during parsing of Fortran source file.
 *
 */
public class FortranException extends Exception
{
    Integer _charIdxInFile;
    Integer _lineIdx;
    Integer _charIdxInLine;

    public Integer getCharIdxInFile()
    {
        return _charIdxInFile;
    }

    public void setCharIdxInFile(Integer val)
    {
        _charIdxInFile = val;
    }

    public Integer getLineIndex()
    {
        return _lineIdx;
    }

    public void setLineIndex(Integer val)
    {
        _lineIdx = val;
    }

    public Integer getCharIdxInLine()
    {
        return _charIdxInLine;
    }

    public void setCharIdxInLine(Integer val)
    {
        _charIdxInLine = val;
    }

    public FortranException(String msg)
    {
        super(msg);
        _charIdxInFile = null;
        _lineIdx = null;
        _charIdxInLine = null;
    }

    public FortranException(String msg, Integer charIdxInFile, Integer lineIdx, Integer charIdxInLine)
    {
        super(msg);
        _charIdxInFile = charIdxInFile;
        _lineIdx = lineIdx;
        _charIdxInLine = charIdxInLine;
    }
}
