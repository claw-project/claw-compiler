/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

import java.util.ArrayList;

import org.antlr.v4.runtime.*;
import java.util.concurrent.CancellationException;

public class ParserErrorListener
    extends BaseErrorListener
{
    public FortranSyntaxException error() { return _error; }
    FortranSyntaxException _error = null;
    
    public void reset() { _error = null; }
    
    @Override
    public void syntaxError(Recognizer<?, ?> recognizer,
                            Object offendingSymbol,
                            int line,
                            int charPositionInLine,
                            String msg,
                            RecognitionException e)
    {
    	_error = new FortranSyntaxException(msg, line, charPositionInLine);
        throw new CancellationException("Syntax error");
    }
}
