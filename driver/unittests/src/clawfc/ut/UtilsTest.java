/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import clawfc.utils.*;
import clawfc.Utils;
import junit.framework.TestCase;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;


public class UtilsTest 
    extends TestCase 
{
    AsciiArrayIOStream.LinesInfo getLinesInfo(String in) throws Exception
    {
        AsciiArrayIOStream strm = new AsciiArrayIOStream();
        InputStream inStrm = new ByteArrayInputStream(in.getBytes(StandardCharsets.US_ASCII));
        Utils.copy(inStrm, strm);
        return strm.getLinesInfo();        
    }
    
    public void testLineInfo() throws Exception
    {
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("");
            assertEquals(lInfo.size(), 0);
            assertEquals(lInfo.numLines(), 0);            
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla");
            assertEquals(lInfo.size(), 3);
            assertEquals(lInfo.numLines(), 1); 
            assertEquals(lInfo.getLineStartByteIdx(0), 0); 
            assertEquals(lInfo.lineByteLength(0), 3);             
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\n");
            assertEquals(lInfo.size(), 4);
            assertEquals(lInfo.numLines(), 1); 
            assertEquals(lInfo.getLineStartByteIdx(0), 0); 
            assertEquals(lInfo.lineByteLength(0), 4);             
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\n\n");
            assertEquals(lInfo.size(), 5);
            assertEquals(lInfo.numLines(), 2); 
            assertEquals(lInfo.getLineStartByteIdx(0), 0); 
            assertEquals(lInfo.lineByteLength(0), 4);   
            assertEquals(lInfo.getLineStartByteIdx(1), 4); 
            assertEquals(lInfo.lineByteLength(1), 1);           
        }
        {
            AsciiArrayIOStream.LinesInfo lInfo = getLinesInfo("bla\nheh");
            assertEquals(lInfo.size(), 7);
            assertEquals(lInfo.numLines(), 2); 
            assertEquals(lInfo.getLineStartByteIdx(0), 0); 
            assertEquals(lInfo.lineByteLength(0), 4);   
            assertEquals(lInfo.getLineStartByteIdx(1), 4); 
            assertEquals(lInfo.lineByteLength(1), 3);           
        }
    }
}
