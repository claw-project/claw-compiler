/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 */
package claw.wani.transformation.sca;

import claw.tatsu.xcodeml.abstraction.AssignStatement;
import claw.tatsu.xcodeml.xnode.common.Xcode;
import claw.tatsu.xcodeml.xnode.common.Xnode;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Class representing a set of contiguous statements that can be wrapped in a do
 * statement and should vectorize.
 *
 * @author clementval
 */
public class VectorBlock
{

    private final Xnode _startStmt;
    private Xnode _endStmt;

    private Set<String> _readAndWrittenVariables = null;
    private Set<String> _writtenVariables = null;

    /**
     * Create a new VectorBlock instance with a single statement.
     *
     * @param startStmt Statement used as the start statement of the created block.
     */
    public VectorBlock(Xnode startStmt)
    {
        _startStmt = startStmt;
        _endStmt = null;
    }

    /**
     * Check whether the VectorBlock is composed of a single statement.
     *
     * @return True if the block is composed by a single statement.
     */
    public boolean isSingleStatement()
    {
        return _endStmt == null;
    }

    /**
     * Set the end statement of the block.
     *
     * @param endStmt Statement to be set as end statement.
     */
    public void setEndStmt(Xnode endStmt)
    {
        _endStmt = endStmt;
    }

    /**
     * Get the start statement of the block.
     *
     * @return Start node.
     */
    public Xnode getStartStmt()
    {
        return _startStmt;
    }

    /**
     * Get the end statement of the block.
     *
     * @return End node.
     */
    public Xnode getEndStmt()
    {
        return _endStmt;
    }

    /**
     * Collect all variables names used within the block.
     */
    private void gatherUsedVariables()
    {

        if (_readAndWrittenVariables == null)
        {
            _readAndWrittenVariables = new HashSet<>();
        }

        if (isSingleStatement())
        {
            _readAndWrittenVariables
                    .addAll(_startStmt.matchAll(Xcode.VAR).stream().map(Xnode::value).collect(Collectors.toList()));
        } else
        {
            Xnode crt = getStartStmt();
            while (!crt.equals(getEndStmt()))
            {
                _readAndWrittenVariables
                        .addAll(crt.matchAll(Xcode.VAR).stream().map(Xnode::value).collect(Collectors.toList()));
                crt = crt.nextSibling();
            }
            if (crt.equals(getEndStmt()))
            {
                _readAndWrittenVariables
                        .addAll(crt.matchAll(Xcode.VAR).stream().map(Xnode::value).collect(Collectors.toList()));
            }
        }
    }

    /**
     * Collect all variables written within the block.
     */
    private void gatherWrittenVariables()
    {
        if (_writtenVariables == null)
        {
            _writtenVariables = new HashSet<>();
        }

        if (isSingleStatement())
        {
            fillUpWrittenVariables(_startStmt);
        } else
        {
            Xnode crt = getStartStmt();
            while (!crt.equals(getEndStmt()))
            {
                fillUpWrittenVariables(crt);
                crt = crt.nextSibling();
            }

            if (crt.equals(getEndStmt()))
            {
                fillUpWrittenVariables(crt);
            }
        }
    }

    private void fillUpWrittenVariables(Xnode root)
    {
        List<Xnode> assignStatements = root.matchAll(Xcode.F_ASSIGN_STATEMENT);
        for (Xnode assign : assignStatements)
        {
            AssignStatement as = new AssignStatement(assign.element());
            _writtenVariables.add(as.getLhsName());
        }
    }

    public Set<String> getWrittenVariables()
    {
        if (_writtenVariables == null)
        {
            gatherWrittenVariables();
        }
        return _writtenVariables;
    }

    /**
     * Get all variable names used within the block.
     *
     * @return Set containing variables names.
     */
    public Set<String> getReadAndWrittenVariables()
    {
        if (_readAndWrittenVariables == null)
        {
            gatherUsedVariables();
        }
        return _readAndWrittenVariables;
    }

    /**
     * Check if the block contains the given assign statement.
     *
     * @param as Assign statement to look for.
     * @return True if the statement is contained in the current block. False
     *         otherwise.
     */
    public boolean contains(AssignStatement as)
    {
        if (isSingleStatement())
        {
            return (Xnode.isOfCode(getStartStmt(), Xcode.F_ASSIGN_STATEMENT) && getStartStmt().equals(as))
                    || (Xnode.isOfCode(getStartStmt(), Xcode.F_IF_STATEMENT) && as.isNestedIn(getStartStmt()));
        } else
        {
            Xnode crtStmt = getStartStmt();
            while (!crtStmt.equals(getEndStmt()))
            {
                if (crtStmt.equals(as))
                {
                    return true;
                }
                crtStmt = crtStmt.nextSibling();
            }
        }
        return false;
    }

    /**
     * Check if the given node is the direct sibling of this node.
     *
     * @param potentialSibling Potential next sibling to test for.
     * @return True if the given node is the direct next sibling. False otherwise.
     */
    private boolean canMergeNextNode(Xnode potentialSibling)
    {
        if (isSingleStatement())
        {
            return getStartStmt().nextSibling() != null && getStartStmt().nextSibling().equals(potentialSibling);
        } else
        {
            return getEndStmt().nextSibling() != null && getEndStmt().nextSibling().equals(potentialSibling);
        }
    }

    /**
     * Merge adjacent block together to maximize vectorization and data locality.
     *
     * @param blocks Set of flagged blocks containing a single statement.
     * @return List of merged blocks.
     */
    public static List<VectorBlock> mergeAdjacent(Set<VectorBlock> blocks)
    {

        List<VectorBlock> sortedVectorBlocks = sortBlockByLineOrder(blocks);
        List<VectorBlock> toBeRemoved = new ArrayList<>();

        if (blocks.isEmpty())
        {
            return sortedVectorBlocks;
        }

        VectorBlock crtBlock = sortedVectorBlocks.get(0);
        for (int i = 1; i < sortedVectorBlocks.size(); ++i)
        {
            VectorBlock nextBlock = sortedVectorBlocks.get(i);
            if (Xnode.isOfCode(nextBlock.getStartStmt(), Xcode.F_ASSIGN_STATEMENT)
                    && crtBlock.canMergeNextNode(nextBlock.getStartStmt()))
            {
                toBeRemoved.add(nextBlock);
                crtBlock.setEndStmt(nextBlock.getStartStmt());
            } else
            {
                crtBlock = nextBlock;
            }
        }

        sortedVectorBlocks.removeAll(toBeRemoved);
        return sortedVectorBlocks;
    }

    /**
     * Sort the vector blocks according to their position in the code.
     *
     * @param blocks Set of vector blocks
     * @return List of ordered vector block.
     */
    private static List<VectorBlock> sortBlockByLineOrder(Set<VectorBlock> blocks)
    {
        List<VectorBlock> sortedVectorBlocks = new ArrayList<>(blocks);
        sortedVectorBlocks.sort((s1, s2) -> {
            if (s1.getStartStmt().lineNo() < s2.getStartStmt().lineNo())
            {
                return -1;
            } else if (s1.getStartStmt().lineNo() > s2.getStartStmt().lineNo())
            {
                return 1;
            }
            return 0;
        });
        return sortedVectorBlocks;
    }

    /**
     * Check if a block contains the given assign statement.
     *
     * @param blocks List of blocks.
     * @param as     Assign statement.
     * @return True if a block contain the statement. False otherwise.
     */
    public static boolean isContainedIn(List<VectorBlock> blocks, AssignStatement as)
    {
        return blocks.stream().anyMatch(b -> b.contains(as));
    }

}
