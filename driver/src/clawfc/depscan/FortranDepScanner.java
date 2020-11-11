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
import clawfc.utils.*;
import java.util.*;

public class FortranDepScanner
{
	FortranCommentsFilter commentsFilter;
	FortranLineBreaksFilter lineBreaksFilter;
	FortranDepParser parser;
	FortranCLAWScanner clawScanner;
    
    public FortranFileBasicSummary basicScan(InputStream input, 
    		                OutputStream outWithoutComments, 
    		                OutputStream outWithoutLineBreaks) throws FortranException, IOException, Exception
    {
    	ByteArrayIOStream inputWithoutComments = new ByteArrayIOStream();
    	commentsFilter.run(input, inputWithoutComments);
    	ByteArrayIOStream inputWithoutLineBreaks = new ByteArrayIOStream(inputWithoutComments.size());
    	InputStream currentInput = inputWithoutComments.getAsInputStream();
    	if(outWithoutComments != null)
    	{
    	    clawfc.Utils.copy(currentInput, outWithoutComments);
    		currentInput.reset();    		
    	}
    	lineBreaksFilter.run(currentInput, inputWithoutLineBreaks, true);
    	currentInput = inputWithoutLineBreaks.getAsInputStream();
    	if(outWithoutLineBreaks != null)
    	{
    	    clawfc.Utils.copy(currentInput, outWithoutLineBreaks);
    		currentInput.reset();    		
    	}
    	FortranFileBasicSummary res = parser.parse(currentInput);
    	return res;    	
    }
    
    public FortranFileBasicSummary basicScan(InputStream input) throws FortranException, IOException, Exception
    {
    	return basicScan(input, null, null);   	
    }
    
    public FortranFileSummary scan(InputStream input) throws FortranException, IOException, Exception
    {
        AsciiArrayIOStream inStrm = new AsciiArrayIOStream();
        clawfc.Utils.copy(input, inStrm);
        FortranFileBasicSummary basicRes = basicScan(inStrm.getAsInputStreamUnsafe(), null, null);    
        AsciiArrayIOStream.LinesInfo linesInfo = inStrm.getLinesInfo();
        FortranFileCLAWLinesInfo clawLinesInfo = clawScanner.run(inStrm.getAsInputStreamUnsafe());
        FortranFileSummary res = new FortranFileSummary(basicRes, linesInfo, clawLinesInfo);
        return res;
    }
	
	public FortranDepScanner() throws Exception
	{
		commentsFilter = new FortranCommentsFilter();
		lineBreaksFilter = new FortranLineBreaksFilter();
		parser = new FortranDepParser();
		clawScanner = new FortranCLAWScanner();
	}
}
