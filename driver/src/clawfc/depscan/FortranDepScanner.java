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
import clawfc.Utils.ByteArrayIOStream;
import java.util.*;

public class FortranDepScanner
{
	FortranCommentsFilter commentsFilter;
	FortranLineBreaksFilter lineBreaksFilter;
	FortranDepParser parser;
    
    public FortranFileSummary scan(InputStream input, 
    		                OutputStream outWithoutComments, 
    		                OutputStream outWithoutLineBreaks) throws FortranException, IOException, Exception
    {
    	ByteArrayIOStream inputWithoutComments = new ByteArrayIOStream();
    	commentsFilter.run(input, inputWithoutComments);
    	ByteArrayIOStream inputWithoutLineBreaks = new ByteArrayIOStream(inputWithoutComments.size());
    	InputStream currentInput = inputWithoutComments.getAsInputStream();
    	/*{
    	    Scanner s = new Scanner(currentInput).useDelimiter("\\A");
    	    String result = s.hasNext() ? s.next() : "";
    	    currentInput.reset();
    	    System.out.println(result + "\n------------------\n");
    	}*/
    	if(outWithoutComments != null)
    	{
    	    clawfc.Utils.copy(currentInput, outWithoutComments);
    		currentInput.reset();    		
    	}
    	lineBreaksFilter.run(currentInput, inputWithoutLineBreaks, true);
    	currentInput = inputWithoutLineBreaks.getAsInputStream();
        /*{
            Scanner s = new Scanner(currentInput).useDelimiter("\\A");
            String result = s.hasNext() ? s.next() : "";
            currentInput.reset();
            System.out.println(result + "\n------------------\n");
        }*/
    	if(outWithoutLineBreaks != null)
    	{
    	    clawfc.Utils.copy(currentInput, outWithoutLineBreaks);
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
