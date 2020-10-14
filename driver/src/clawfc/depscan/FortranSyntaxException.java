/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import clawfc.depscan.parser.*;

public class FortranSyntaxException
	extends FortranException
{    
	public FortranSyntaxException(String msg, int line, int charPositionInLine)
	{
	    super(msg, line, charPositionInLine);
	}
}