/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

public class FortranSyntaxException extends FortranException
{
    public FortranSyntaxException(String msg, Integer charIdxInFile, Integer lineIdx, Integer charIdxInLine)
    {
        super(msg, charIdxInFile, lineIdx, charIdxInLine);
    }
}
