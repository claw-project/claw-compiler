/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import clawfc.Utils;

/**
 * Stores sequences removed by the Filter
 */
public class FilteredContent
{
    class Data implements Comparable<Integer>
    {
        public final int startChrIdx;
        public final int endChrIdxFiltered;
        public final int offset;
        public final String content;

        public Data(int startChrIdx, String content, int offset)
        {
            this.startChrIdx = startChrIdx;
            this.endChrIdxFiltered = startChrIdx - offset;
            this.content = content;
            this.offset = offset + this.content.length();
        }

        @Override
        public int compareTo(Integer chrIdxAfter)
        {
            return Integer.compare(endChrIdxFiltered, chrIdxAfter);
        }
    }

    final List<Data> data;
    int offset;

    public int numSeqs()
    {
        return data.size();
    }

    public String getSequenceContent(int idx)
    {
        return data.get(idx).content;
    }

    public int getSequenceStartChrIdx(int idx)
    {
        return data.get(idx).startChrIdx;
    }

    public FilteredContent()
    {
        data = new ArrayList<Data>();
        offset = 0;
    }

    public void addSequence(int startChrIdx, String content)
    {
        data.add(new Data(startChrIdx, content, offset));
        offset += content.length();
    }

    /**
     * @param filteredChrIdx Character index in the filtered file
     * @return Character index in the pre-filtered file
     */
    public int getOriginalChrIdx(int filteredChrIdx)
    {
        int offset = 0;
        if (!data.isEmpty())
        {
            int idx = Collections.binarySearch(data, filteredChrIdx);
            if (idx < 0)
            {
                // Check Collections.binarySearch doc for explanation
                int decodedIdx = -idx - 2;
                idx = Integer.min(decodedIdx, data.size() - 1);
            }
            if (idx >= 0)
            {
                offset = data.get(idx).offset;
            }
        }
        return filteredChrIdx + offset;
    }

    public void reverse(InputStream filteredText, OutputStream unFilteredText) throws IOException
    {
        int chrIdxFiltered = 0;
        for (Data fSeq : data)
        {
            int nBytes = fSeq.endChrIdxFiltered - chrIdxFiltered;
            byte[] bytes = filteredText.readNBytes(nBytes);
            unFilteredText.write(bytes);
            unFilteredText.write(fSeq.content.getBytes(StandardCharsets.US_ASCII));
        }
        Utils.copy(filteredText, unFilteredText);
    }
}
