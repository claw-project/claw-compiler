/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Stores sequences altered by the Filter
 */
public interface FilteredContent
{
    public int numSeqs();

    public String getSequenceContent(int idx);

    public void addSequence(int startChrIdx, String content);

    /**
     * @param chrIdxFiltered Character index in the filtered file
     * @return Character index in the pre-filtered file or null if no such index
     *         exists
     */
    public Integer getOriginalChrIdx(int chrIdxFiltered);

    public Integer getChrIdxFiltered(int chrIdx);

    public void reverse(InputStream filteredText, OutputStream unFilteredText) throws IOException;
}
