/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

public class FortranSemanticException extends FortranException
{
    public FortranSemanticException(String msg, Integer charIdxInFile, Integer lineIdx, Integer charIdxInLine)
    {
        super(msg, charIdxInFile, lineIdx, charIdxInLine);
    }
    public FortranSemanticException(String msg, Integer charIdxInFile)
    {
        super(msg, charIdxInFile, null, null);
    }

    public FortranSemanticException(String msg)
    {
        super(msg);
    }
}
