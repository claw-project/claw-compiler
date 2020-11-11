/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import java.util.*;

public class AsciiArrayIOStream
    extends ByteArrayIOStream
{
    public static class LinesInfo
    {
        final int _size;
        final int[] _lineStartByteIdx;

        public int size() 
        { return _size; }
        
        public int numLines() 
        { return _lineStartByteIdx.length; }
        
        public int lineStartByteIdx(int lineIdx) 
        { return _lineStartByteIdx[lineIdx]; } 
        
        public int lineEndByteIdx(int lineIdx) 
        { 
            if(lineIdx == (numLines() - 1))
            { return  _size; }
            else
            { return _lineStartByteIdx[lineIdx + 1]; } 
        } 
        
        public int lineByteLength(int lineIdx) 
        { 
            return lineEndByteIdx(lineIdx) - lineStartByteIdx(lineIdx);
        } 
        
        public LinesInfo(byte[] buf, int size)
        {
            _size = size;
            if(size == 0)
            {
                _lineStartByteIdx = new int[0];
            }
            else
            {
                final byte EOL = '\n';
                List<Integer> startIdx = new ArrayList<Integer>();
                int idx = 0;
                for(int i = 0; i < size; ++i)
                {
                    if(buf[i] == EOL)
                    { 
                        startIdx.add(idx);
                        idx = i + 1;
                    }
                }
                if(idx < size)
                { startIdx.add(idx); } 
                final int n = startIdx.size();
                _lineStartByteIdx = new int[n];
                for(int i = 0; i < n; ++i)
                {
                    _lineStartByteIdx[i] = startIdx.get(i);
                }
            }
        }        
    }
    
    public LinesInfo getLinesInfo()
    {
        return new LinesInfo(this.buf, this.count);
    }
}
