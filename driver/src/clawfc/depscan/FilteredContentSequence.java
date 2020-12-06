/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class FilteredContentSequence
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

    List<FilteredContent> data;

    public int size()
    {
        return data.size();
    }

    public FilteredContentSequence get(int idx)
    {
        return new FilteredContentSequence(Arrays.asList(data.get(idx)));
    }

    public FilteredContentSequence(List<FilteredContent> data)
    {
        this.data = data;
    }

    public FilteredContentSequence()
    {
        this.data = new ArrayList<FilteredContent>();
    }

    public void add(FilteredContent el)
    {
        data.add(el);
    }

    public void add(FilteredContentSequence seq)
    {
        data.addAll(seq.data);
    }

    public static FilteredContentSequence decomposeIntoSeqs(List<Op> ops)
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
        FilteredContentSequence res = new FilteredContentSequence(
                Collections.unmodifiableList(Arrays.asList(add, rem)));
        return res;
    }

    public static String toString(List<Op> ops)
    {
        StringBuilder sb = new StringBuilder();
        for (Op op : ops)
        {
            if (op.type == OpType.Add)
            {
                sb.append("add ");
            } else
            {
                sb.append("rem ");
            }
            sb.append(op.pos).append(" \"").append(op.txt).append("\"\n");
        }
        return sb.toString();
    }

    /**
     * Reverses the rearrangement effects of the successively applied sequence of
     * filters
     */
    public Integer getOriginalChrIdx(int chrIdxFiltered)
    {
        if (chrIdxFiltered < 0)
        {
            return null;
        }
        Integer idx = chrIdxFiltered;
        for (int i = data.size() - 1; i >= 0; --i)
        {
            FilteredContent content = data.get(i);
            Integer newIdx = content.getOriginalChrIdx(idx);
            idx = newIdx;
            if (idx == null)
            {
                return idx;
            }
        }
        return idx;
    }

    /**
     * Reverses the rearrangement effects of the successively applied sequence of
     * filters
     */
    public FortranStatementBasicPosition getOriginal(FortranStatementBasicPosition basicPos)
    {
        int newStartChrIdx = getOriginalChrIdx(basicPos.getStartCharIdx());
        int newEndChrIdx = getOriginalChrIdx(basicPos.getEndCharIdx() - 1) + 1;// Index is not inclusive
        return new FortranStatementBasicPosition(basicPos.getName(), newStartChrIdx, newEndChrIdx);
    }

    public String partialReverse(String filteredStr, char fillChr)
    {
        Map<Integer, Character> res = new LinkedHashMap<Integer, Character>();
        int lastIdx = -1;
        for (int i = 0; i < filteredStr.length(); ++i)
        {
            Integer idx = getOriginalChrIdx(i);
            if (idx != null)
            {
                if (idx < lastIdx)
                {
                    throw new RuntimeException();
                }
                lastIdx = idx;
                if (res.put(idx, filteredStr.charAt(i)) != null)
                {
                    throw new RuntimeException();
                }
            }
        }

        StringBuilder sB = new StringBuilder(lastIdx + 1);
        for (int i = 0; i <= lastIdx; ++i)
        {
            Character chr = res.get(i);
            if (chr == null)
            {
                sB.append(fillChr);
            } else
            {
                sB.append(chr);
            }

        }
        return sB.toString();
    }
}
