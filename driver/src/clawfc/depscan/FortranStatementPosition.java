/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static java.lang.Math.toIntExact;

import clawfc.utils.AsciiArrayIOStream;

public class FortranStatementPosition
{
    clawfc.depscan.serial.FortranStatementPosition data;

    public FortranStatementPosition(String name, int startCharIdx, int endCharIdx, int startLineIdx, int endLineIdx)
    {
        data = new clawfc.depscan.serial.FortranStatementPosition();
        data.setName(name);
        data.setStartCharIdx(startCharIdx);
        data.setEndCharIdx(endCharIdx);
        data.setStartLineIdx(startLineIdx);
        data.setEndLineIdx(endLineIdx);
    }

    public FortranStatementPosition(clawfc.depscan.serial.FortranStatementPosition data)
    {
        this.data = data;
    }

    public clawfc.depscan.serial.FortranStatementPosition getData()
    {
        return data;
    }

    public String getName()
    {
        return data.getName();
    }

    public int getStartCharIdx()
    {
        return toIntExact(data.getStartCharIdx());
    }

    public int getEndCharIdx()
    {
        return toIntExact(data.getEndCharIdx());
    }

    public int getStartLineIdx()
    {
        return toIntExact(data.getStartLineIdx());
    }

    public int getEndLineIdx()
    {
        return toIntExact(data.getEndLineIdx());
    }

    @Override
    public boolean equals(Object obj)
    {
        if (this == obj)
        {
            return true;
        }
        if (obj == null)
        {
            return false;
        }
        if (getClass() != obj.getClass())
        {
            return false;
        }
        FortranStatementPosition other = (FortranStatementPosition) obj;
        if (!equals(data, other.data))
        {
            return false;
        }
        return true;
    }

    public static boolean equals(clawfc.depscan.serial.FortranStatementPosition p1,
            clawfc.depscan.serial.FortranStatementPosition p2)
    {
        if (!p1.getName().equals(p2.getName()))
        {
            return false;
        }
        if (p1.getStartCharIdx() != p2.getStartCharIdx())
        {
            return false;
        }
        if (p1.getEndCharIdx() != p2.getEndCharIdx())
        {
            return false;
        }
        if (p1.getStartLineIdx() != p2.getStartLineIdx())
        {
            return false;
        }
        if (p1.getEndLineIdx() != p2.getEndLineIdx())
        {
            return false;
        }
        return true;
    }

    public static clawfc.depscan.FortranStatementPosition createPosition(FortranStatementBasicPosition basicPos,
            AsciiArrayIOStream.LinesInfo linesInfo)
    {
        int startLineIdx = linesInfo.getLineIdx(basicPos.getStartCharIdx());
        int endLineIdx = linesInfo.getLineIdx(basicPos.getEndCharIdx());
        ++endLineIdx;// EndCharIdx is always on the same line as the last symbol
        return new clawfc.depscan.FortranStatementPosition(basicPos.getName(), basicPos.getStartCharIdx(),
                basicPos.getEndCharIdx(), startLineIdx, endLineIdx);
    }
}
