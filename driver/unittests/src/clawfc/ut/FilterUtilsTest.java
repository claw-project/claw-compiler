/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.ut;

import java.util.Arrays;
import java.util.List;

import clawfc.depscan.FilteredContentSequence;
import clawfc.depscan.FilteredContentSequence.Op;
import clawfc.depscan.FilteredContentSequence.OpType;
import clawfc.depscan.InsertedFilteredContent;
import clawfc.depscan.RemovedFilteredContent;
import junit.framework.TestCase;

public class FilterUtilsTest extends TestCase
{
    public void testRemovedContent()
    {
        RemovedFilteredContent c = new RemovedFilteredContent();
        assertEquals(null, c.getOriginalChrIdx(-1));
        assertEquals(Integer.valueOf(0), c.getOriginalChrIdx(0));
        assertEquals(Integer.valueOf(10), c.getOriginalChrIdx(10));
        c.addSequence(0, "01");
        assertEquals(null, c.getOriginalChrIdx(-1));
        for (int i = 0; i < 10; ++i)
        {
            assertEquals(Integer.valueOf(i + 2), c.getOriginalChrIdx(i));
        }
        for (int i = 0; i < 2; ++i)
        {
            boolean exCaught = false;
            try
            {
                c.addSequence(i, "01");
            } catch (RuntimeException e)
            {
                exCaught = true;
            }
            assertTrue(exCaught);
        }
        // c.addSequence(2, "23"); Currently not supported
        c.addSequence(3, "34");
        assertEquals(null, c.getOriginalChrIdx(-1));
        assertEquals(Integer.valueOf(2), c.getOriginalChrIdx(0));
        for (int i = 1; i < 10; ++i)
        {
            assertEquals(Integer.valueOf(i + 4), c.getOriginalChrIdx(i));
        }
    }

    public void testInsertedContent()
    {
        InsertedFilteredContent c = new InsertedFilteredContent();
        assertEquals(null, c.getChrIdxFiltered(-1));
        assertEquals(Integer.valueOf(0), c.getChrIdxFiltered(0));
        assertEquals(Integer.valueOf(10), c.getChrIdxFiltered(10));
        assertEquals(null, c.getOriginalChrIdx(-1));
        assertEquals(Integer.valueOf(0), c.getOriginalChrIdx(0));
        assertEquals(Integer.valueOf(10), c.getOriginalChrIdx(10));
        // -----------------------------------
        c.addSequence(0, "12");
        assertEquals(null, c.getChrIdxFiltered(-1));
        assertEquals(Integer.valueOf(2), c.getChrIdxFiltered(0));
        assertEquals(Integer.valueOf(3), c.getChrIdxFiltered(1));
        assertEquals(Integer.valueOf(12), c.getChrIdxFiltered(10));
        assertEquals(null, c.getOriginalChrIdx(-1));
        assertEquals(null, c.getOriginalChrIdx(0));
        assertEquals(null, c.getOriginalChrIdx(1));
        assertEquals(Integer.valueOf(0), c.getOriginalChrIdx(2));
        assertEquals(Integer.valueOf(10), c.getOriginalChrIdx(12));
        // -----------------------------------
        boolean exCaught = false;
        try
        {
            c.addSequence(0, "34");
        } catch (RuntimeException e)
        {
            exCaught = true;
        }
        assertTrue(exCaught);
        c.addSequence(1, "34");
        assertEquals(null, c.getChrIdxFiltered(-1));
        // [1 2] 0 [3 4] 1 2
        assertEquals(Integer.valueOf(2), c.getChrIdxFiltered(0));
        assertEquals(Integer.valueOf(5), c.getChrIdxFiltered(1));
        for (int i = 2; i < 10; ++i)
        {
            assertEquals(Integer.valueOf(i + 4), c.getChrIdxFiltered(i));
        }
        for (int i = -1; i < 2; ++i)
        {
            assertEquals(null, c.getOriginalChrIdx(i));
        }
    }

    public void testOperationSequence()
    {
        {
            List<Op> emptySeq = Arrays.asList();
            FilteredContentSequence contents = FilteredContentSequence.decomposeIntoSeqs(emptySeq);
            assertEquals(null, contents.getOriginalChrIdx(-1));
            for (int i = 0; i < 10; ++i)
            {
                assertEquals(Integer.valueOf(i), contents.getOriginalChrIdx(i));
            }
        }
        {
            List<Op> seq = Arrays.asList(new Op(OpType.Add, 0, "01"));
            FilteredContentSequence contents = FilteredContentSequence.decomposeIntoSeqs(seq);
            assertEquals(null, contents.getOriginalChrIdx(-1));
            for (int i = 0; i < 2; ++i)
            {
                assertEquals(null, contents.getOriginalChrIdx(i));
            }
            for (int i = 3; i < 10; ++i)
            {
                assertEquals(Integer.valueOf(i - 2), contents.getOriginalChrIdx(i));
            }
        }
        {
            List<Op> seq = Arrays.asList(new Op(OpType.Remove, 0, "01"));
            FilteredContentSequence contents = FilteredContentSequence.decomposeIntoSeqs(seq);
            assertEquals(null, contents.getOriginalChrIdx(-1));
            for (int i = 0; i < 10; ++i)
            {
                assertEquals(Integer.valueOf(i + 2), contents.getOriginalChrIdx(i));
            }
        }
        {

//          original:       rr2345 
//          after addition: aarr2345 
//          after removal:  aa2345 
//          after reversal: __2345

            List<Op> seq = Arrays.asList(new Op(OpType.Add, 0, "aa"), new Op(OpType.Remove, 0, "rr"));
            FilteredContentSequence contents = FilteredContentSequence.decomposeIntoSeqs(seq);
            assertEquals(null, contents.getOriginalChrIdx(-1));
            for (int i = 0; i < 2; ++i)
            {
                assertEquals(null, contents.getOriginalChrIdx(i));
            }
            for (int i = 2; i < 10; ++i)
            {
                assertEquals(Integer.valueOf(i), contents.getOriginalChrIdx(i));
            }
        }
        {

//          original:       rr23rr67 
//          after addition: aarr23rr67aa
//          after removal:  aa2367aa 
//          after reversal: __2367__
            List<Op> seq = Arrays.asList(new Op(OpType.Remove, 0, "rr"), new Op(OpType.Remove, 4, "rr"),
                    new Op(OpType.Add, 0, "aa"), new Op(OpType.Add, 8, "aa"));
            FilteredContentSequence contents = FilteredContentSequence.decomposeIntoSeqs(seq);
            assertEquals(null, contents.getOriginalChrIdx(-1));
            for (int i = 0; i < 2; ++i)
            {
                assertEquals(null, contents.getOriginalChrIdx(i));
            }
            for (int i = 2; i < 3; ++i)
            {
                assertEquals(Integer.valueOf(i), contents.getOriginalChrIdx(i));
            }
            for (int i = 4; i < 5; ++i)
            {
                assertEquals(Integer.valueOf(i + 2), contents.getOriginalChrIdx(i));
            }
            for (int i = 6; i < 7; ++i)
            {
                assertEquals(null, contents.getOriginalChrIdx(i));
            }
            for (int i = 8; i < 10; ++i)
            {
                assertEquals(Integer.valueOf(i), contents.getOriginalChrIdx(i));
            }
        }
    }

}
