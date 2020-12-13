/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.firstGreater;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
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

        boolean contains(int chrIdxFiltered)
        {
            return chrIdxFiltered >= startChrIdxFiltered && chrIdxFiltered < endChrIdxFiltered;
        }

        @Override
        public int compareTo(Integer chrIdxFiltered)
        {
            return Integer.compare(startChrIdxFiltered, chrIdxFiltered);
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
        if (!data.isEmpty())
        {
            final int idx = firstGreater(data, chrIdxFiltered) - 1;
            if (idx >= 0)
            {
                Data d = data.get(idx);
                if (d.contains(chrIdxFiltered))
                {// This means chrIdxFiltered is within one of the added sequences, which don't
                 // exist in original text
                    return null;
                } else
                {
                    chrIdxFiltered -= d.offset;
                }
            }
        }
        return chrIdxFiltered;
    }

    @Override
    public Integer getChrIdxFiltered(int chrIdx)
    {
        if (chrIdx < 0)
        {
            return null;
        }
        if (!data.isEmpty())
        {
            int idx = firstGreater(this.startChrIndex, chrIdx) - 1;
            if (idx >= 0 && idx < data.size())
            {
                int offset = data.get(idx).offset;
                chrIdx += offset;
            }
        }
        return chrIdx;
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
