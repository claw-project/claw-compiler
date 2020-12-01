/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

public class FortranSyntaxException extends FortranException
{
    public FortranSyntaxException(String msg, Integer charIdxInFile, Integer lineIdx, Integer charIdxInLine)
    {
        super(msg, charIdxInFile, lineIdx, charIdxInLine);
    }
}