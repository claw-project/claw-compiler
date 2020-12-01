/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

public class FortranSemanticException extends FortranException
{
    public FortranSemanticException(String msg, Integer charIdxInFile, Integer lineIdx, Integer charIdxInLine)
    {
        super(msg, charIdxInFile, lineIdx, charIdxInLine);
    }

    public FortranSemanticException(String msg)
    {
        super(msg);
    }
}
