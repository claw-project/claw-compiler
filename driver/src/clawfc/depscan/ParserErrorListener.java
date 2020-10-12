/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

import java.util.ArrayList;

import org.antlr.v4.runtime.*;

public class ParserErrorListener
    extends BaseErrorListener
{
    public ArrayList<FortranSourceRecognitionException> errors = new ArrayList<FortranSourceRecognitionException>();
    
    public void reset() { errors.clear(); }
    
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                            Object offendingSymbol,
                            int line,
                            int charPositionInLine,
                            String msg,
                            RecognitionException e)
    {
        FortranSourceRecognitionException ex = new FortranSourceRecognitionException(msg, line, charPositionInLine);
        errors.add(ex);
    }
}
