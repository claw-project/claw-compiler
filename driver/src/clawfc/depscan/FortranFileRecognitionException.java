/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

public class FortranFileRecognitionException
    extends FortranSourceRecognitionException
{
    String _filename;
    
    public String filename() { return _filename; }
    
    public FortranFileRecognitionException(FortranSourceRecognitionException strmException, String filename)
    {
        super(strmException);
        _filename = filename;
    }
}
