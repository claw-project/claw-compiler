/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class FilterUtils
{
    public enum OpType {
        Add, Remove
    }

    public static class Op
    {
        public final OpType type;
        public final int pos;
        public final String txt;

        public Op(OpType type, int pos, String txt)
        {
            this.type = type;
            this.pos = pos;
            this.txt = txt;
        }
    }

    public static List<FilteredContent> decomposeIntoSeqs(List<Op> ops)
    {
        InsertedFilteredContent add = new InsertedFilteredContent();
        RemovedFilteredContent rem = new RemovedFilteredContent();
        for (Op op : ops)
        {
            if (op.type == OpType.Add)
            {
                add.addSequence(op.pos, op.txt);
            }
        }
        for (Op op : ops)
        {
            if (op.type == OpType.Remove)
            {
                int newPos = add.getChrIdxFiltered(op.pos);
                rem.addSequence(newPos, op.txt);
            }
        }
        List<FilteredContent> res = Collections.unmodifiableList(Arrays.asList(add, rem));
        return res;
    }

    /**
     * Reverses the rearrangement effects of the successively applied sequence of
     * filters
     */
    public static Integer getOriginalChrIdx(List<FilteredContent> filteredContentsSeq, int chrIdxFiltered)
    {
        if (chrIdxFiltered < 0)
        {
            return null;
        }
        Integer idx = chrIdxFiltered;
        for (int i = filteredContentsSeq.size() - 1; i >= 0; --i)
        {
            FilteredContent content = filteredContentsSeq.get(i);
            idx = content.getOriginalChrIdx(idx);
            if (idx == null)
            {
                return idx;
            }
        }
        return idx;
    }
}
