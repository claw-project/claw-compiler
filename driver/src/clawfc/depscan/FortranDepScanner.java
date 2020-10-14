/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class FortranDepScanner
{
	FortranCommentsFilter commentsFilter;
	FortranLineBreaksFilter lineBreaksFilter;
	FortranDepParser parser;
    
    public FortranFileSummary scan(InputStream input, 
    		                OutputStream outWithoutComments, 
    		                OutputStream outWithoutLineBreaks) throws FortranException, IOException, Exception
    {
    	Utils.ByteArrayIOStream inputWithoutComments = new Utils.ByteArrayIOStream();
    	commentsFilter.run(input, inputWithoutComments);
    	Utils.ByteArrayIOStream inputWithoutLineBreaks = new Utils.ByteArrayIOStream(inputWithoutComments.size());
    	InputStream currentInput = inputWithoutComments.getAsInputStream();
    	if(outWithoutComments != null)
    	{
    		Utils.copy(currentInput, outWithoutComments);
    		currentInput.reset();    		
    	}
    	lineBreaksFilter.run(currentInput, inputWithoutLineBreaks);
    	currentInput = inputWithoutLineBreaks.getAsInputStream();
    	if(outWithoutLineBreaks != null)
    	{
    		Utils.copy(currentInput, outWithoutLineBreaks);
    		currentInput.reset();    		
    	}
    	FortranFileSummary res = parser.parse(currentInput);
    	return res;    	
    }
    
    public FortranFileSummary scan(InputStream input) throws FortranException, IOException, Exception
    {
    	return scan(input, null, null);   	
    }
	
	public FortranDepScanner() throws Exception
	{
		commentsFilter = new FortranCommentsFilter();
		lineBreaksFilter = new FortranLineBreaksFilter();
		parser = new FortranDepParser();
	}
}
