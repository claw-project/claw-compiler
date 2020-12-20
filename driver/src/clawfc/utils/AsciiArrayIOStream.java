/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.utils;

import static clawfc.Utils.ASCII_NEWLINE_VALUE;
import static clawfc.Utils.copy;
import static clawfc.Utils.firstGreater;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class AsciiArrayIOStream extends ByteArrayIOStream
{
    public static class LinesInfo
    {
        final int _size;
        final int[] _lineStartCharIdx;

        public int size()
        {
            return _size;
        }

        public int numLines()
        {
            return _lineStartCharIdx.length;
        }

        public int getLineStartByteIdx(int lineIdx)
        {
            return _lineStartCharIdx[lineIdx];
        }

        public int getLineEndByteIdx(int lineIdx)
        {
            if (lineIdx == (numLines() - 1))
            {
                return _size;
            } else
            {
                return _lineStartCharIdx[lineIdx + 1];
            }
        }

        public int lineByteLength(int lineIdx)
        {
            return getLineEndByteIdx(lineIdx) - getLineStartByteIdx(lineIdx);
        }

        public Integer getLineIdx(int charIdx)
        {
            if (charIdx < 0)
            {
                return null;
            } else if (charIdx >= size())
            {
                return size();
            } else
            {
                int lineIdx = firstGreater(_lineStartCharIdx, charIdx) - 1;
                return lineIdx;
            }
        }

        public Integer getLineChrOffset(int charIdx)
        {
            if (charIdx < 0 || charIdx >= size())
            {
                return null;
            } else
            {
                int lineIdx = firstGreater(_lineStartCharIdx, charIdx) - 1;
                return charIdx - _lineStartCharIdx[lineIdx];
            }
        }

        public LinesInfo(byte[] buf, int size)
        {
            _size = size;
            if (size == 0)
            {
                _lineStartCharIdx = new int[0];
            } else
            {
                final byte EOL = '\n';
                List<Integer> startIdx = new ArrayList<Integer>();
                int idx = 0;
                for (int i = 0; i < size; ++i)
                {
                    if (buf[i] == EOL)
                    {
                        startIdx.add(idx);
                        idx = i + 1;
                    }
                }
                if (idx < size)
                {
                    startIdx.add(idx);
                }
                final int n = startIdx.size();
                _lineStartCharIdx = new int[n];
                for (int i = 0; i < n; ++i)
                {
                    _lineStartCharIdx[i] = startIdx.get(i);
                }
            }
        }
    }

    public static LinesInfo getLinesInfo(Path filePath) throws IOException
    {
        try (InputStream inStrm = Files.newInputStream(filePath))
        {
            return getLinesInfo(inStrm, false);
        }
    }

    public static LinesInfo getLinesInfo(InputStream inStrm) throws IOException
    {
        return getLinesInfo(inStrm, true);
    }

    public static LinesInfo getLinesInfo(InputStream inStrm, boolean resetInputStream) throws IOException
    {
        LinesInfo linesInfo = null;
        if (resetInputStream)
        {
            inStrm.reset();
        }
        AsciiArrayIOStream asciiStrm = new AsciiArrayIOStream();
        copy(inStrm, asciiStrm);
        linesInfo = asciiStrm.getLinesInfo();
        return linesInfo;
    }

    public LinesInfo getLinesInfo()
    {
        return new LinesInfo(this.buf, this.count);
    }

    public AsciiArrayIOStream()
    {
        super();
    }

    public AsciiArrayIOStream(int capacity)
    {
        super(capacity);
    }

    public AsciiArrayIOStream(Path filePath) throws IOException
    {
        super(filePath);
    }

    public AsciiArrayIOStream(String str) throws IOException
    {
        super(str.getBytes(StandardCharsets.US_ASCII));
    }

    public Integer findLineStartChrIdx(int chrIdx)
    {
        if (chrIdx >= 0 && chrIdx < count)
        {
            for (int i = chrIdx; i >= 1; --i)
            {
                if (buf[i - 1] == ASCII_NEWLINE_VALUE)
                {
                    return i;
                }
            }
            return 0;
        } else
        {
            return null;
        }

    }

    public int size()
    {
        return count;
    }

    public Byte getChr(int chrIdx)
    {
        if (chrIdx >= 0 && chrIdx < count)
        {
            return buf[chrIdx];
        } else
        {
            return null;
        }

    }
}
