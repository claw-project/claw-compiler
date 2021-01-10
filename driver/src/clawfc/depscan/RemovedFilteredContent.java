/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import static clawfc.Utils.firstGreater;
import static clawfc.depscan.Utils.readNBytes;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

import clawfc.Utils;

/**
 * Stores sequences removed by the Filter
 */
public class RemovedFilteredContent implements FilteredContent
{
    class Data implements Comparable<Integer>
    {
        public final int startChrIdx;
        public final int endChrIdxFiltered;
        public final int offset;
        public final String content;

        public int startChrEndIdx()
        {
            return startChrIdx + content.length();
        }

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

    @Override
    public int numSeqs()
    {
        return data.size();
    }

    @Override
    public String getSequenceContent(int idx)
    {
        return data.get(idx).content;
    }

    public int getSequenceStartChrIdx(int idx)
    {
        return data.get(idx).startChrIdx;
    }

    public RemovedFilteredContent()
    {
        data = new ArrayList<Data>();
        offset = 0;
    }

    @Override
    public void addSequence(int startChrIdx, String content)
    {
        if (content.isEmpty())
        {
            return;
        }
        if (startChrIdx < 0 || (!data.isEmpty() && startChrIdx <= data.get(data.size() - 1).startChrEndIdx()))
        {
            throw new RuntimeException("Addition can be only AFTER the end of the sequence");
        }
        data.add(new Data(startChrIdx, content, offset));
        offset += content.length();
    }

    @Override
    public Integer getOriginalChrIdx(int chrIdxFiltered)
    {
        if (chrIdxFiltered < 0)
        {
            return null;
        }
        if (!data.isEmpty())
        {
            int idx = firstGreater(data, chrIdxFiltered) - 1;
            if (idx >= 0)
            {
                Data d = data.get(idx);
                chrIdxFiltered += d.offset;
            }
        }
        return chrIdxFiltered;
    }

    @Override
    public Integer getChrIdxFiltered(int chrIdx)
    {
        throw new RuntimeException("Not implemented");
    }

    @Override
    public void reverse(InputStream filteredText, OutputStream unFilteredText) throws IOException
    {
        int chrIdxFiltered = 0;
        for (Data fSeq : data)
        {
            int nBytes = fSeq.endChrIdxFiltered - chrIdxFiltered;
            byte[] bytes = readNBytes(filteredText, nBytes);
            unFilteredText.write(bytes);
            unFilteredText.write(fSeq.content.getBytes(StandardCharsets.US_ASCII));
        }
        Utils.copy(filteredText, unFilteredText);
    }
}
