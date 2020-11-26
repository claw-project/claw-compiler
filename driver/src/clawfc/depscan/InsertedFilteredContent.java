/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import clawfc.Utils;

/**
 * Stores sequences added by the Filter
 */
public class InsertedFilteredContent implements FilteredContent
{
    class Data implements Comparable<Integer>
    {
        public final int startChrIdxFiltered;
        public final int endChrIdxFiltered;
        public final int offset;
        public final String content;

        public Data(int startChrIdx, String content, int offset)
        {
            startChrIdxFiltered = startChrIdx + offset;
            endChrIdxFiltered = startChrIdxFiltered + content.length();
            this.offset = offset + content.length();
            this.content = content;
        }

        @Override
        public int compareTo(Integer chrIdxFiltered)
        {
            if (chrIdxFiltered < startChrIdxFiltered)
            {
                return 1;
            } else if (chrIdxFiltered >= endChrIdxFiltered)
            {
                return -1;
            } else
            {
                return 0;
            }
        }
    }

    final List<Data> data;
    final List<Integer> startChrIndex;
    int offset;

    public int numSeqs()
    {
        return data.size();
    }

    public String getSequenceContent(int idx)
    {
        return data.get(idx).content;
    }

    public InsertedFilteredContent()
    {
        data = new ArrayList<Data>();
        startChrIndex = new ArrayList<Integer>();
        offset = 0;
    }

    public void addSequence(int startChrIdx, String content)
    {
        if (content.isEmpty())
        {
            return;
        }
        if (startChrIdx < 0 || (!data.isEmpty() && this.startChrIndex.get(data.size() - 1) >= startChrIdx))
        {
            throw new RuntimeException("Addition can be only AFTER the end of the sequence");
        }
        data.add(new Data(startChrIdx, content, offset));
        this.startChrIndex.add(startChrIdx);
        offset += content.length();
    }

    /**
     * @param chrIdxFiltered Character index in the filtered file
     * @return Character index in the pre-filtered file or null if no such index
     *         exists
     */
    public Integer getOriginalChrIdx(int chrIdxFiltered)
    {
        if (chrIdxFiltered < 0)
        {
            return null;
        }
        int offset = 0;
        if (!data.isEmpty())
        {
            final int srchIdx = Collections.binarySearch(data, chrIdxFiltered);
            if (srchIdx >= 0)
            {// This means chrIdxFiltered is within one of the added sequences, which don't
             // exist in original text
                return null;
            } else
            {
                // Check Collections.binarySearch doc for explanation
                // int insPoint = -srchIdx - 1;
                int seqIdx = -srchIdx - 2;
                offset = data.get(seqIdx).offset;
            }
        }
        return chrIdxFiltered - offset;
    }

    @Override
    public Integer getChrIdxFiltered(int chrIdx)
    {
        if (chrIdx < 0)
        {
            return null;
        }
        if (data.isEmpty())
        {
            return chrIdx;
        }
        int idx = Collections.binarySearch(this.startChrIndex, chrIdx);
        if (idx >= 0)
        {
            offset = data.get(idx).offset;
        } else
        {
            // Check Collections.binarySearch doc for explanation
            int decodedIdx = -idx - 2;
            if (decodedIdx < 0)
            {
                return chrIdx;
            }
            idx = Integer.min(decodedIdx, data.size() - 1);
            offset = data.get(idx).offset;
        }
        return offset + chrIdx;
    }

    public void reverse(InputStream filteredText, OutputStream unFilteredText) throws IOException
    {
        int chrIdxFiltered = 0;
        for (Data fSeq : data)
        {
            int nBytes = fSeq.startChrIdxFiltered - chrIdxFiltered;
            byte[] bytes = filteredText.readNBytes(nBytes);
            unFilteredText.write(bytes);
            chrIdxFiltered += nBytes;
            int fSeqLen = fSeq.content.length();
            filteredText.readNBytes(fSeqLen);
            chrIdxFiltered += fSeqLen;
        }
        Utils.copy(filteredText, unFilteredText);
    }
}
