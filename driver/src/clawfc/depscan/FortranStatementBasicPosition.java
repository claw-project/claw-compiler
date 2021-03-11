/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static java.lang.Math.toIntExact;

public class FortranStatementBasicPosition
{
    clawfc.depscan.serial.FortranStatementBasicPosition data;

    public FortranStatementBasicPosition(String name, int startCharIdx, int endCharIdx)
    {
        data = new clawfc.depscan.serial.FortranStatementBasicPosition();
        data.setStartCharIdx(startCharIdx);
        data.setEndCharIdx(endCharIdx);
        data.setName(name);
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

    public int length()
    {
        return getEndCharIdx() - getStartCharIdx();
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
        FortranStatementBasicPosition other = (FortranStatementBasicPosition) obj;
        if (!equals(data, other.data))
        {
            return false;
        }
        return true;
    }

    public static boolean equals(clawfc.depscan.serial.FortranStatementBasicPosition p1,
            clawfc.depscan.serial.FortranStatementBasicPosition p2)
    {
        if (p1.getStartCharIdx() != p2.getStartCharIdx())
        {
            return false;
        }
        if (p1.getEndCharIdx() != p2.getEndCharIdx())
        {
            return false;
        }
        if (!p1.getName().equals(p2.getName()))
        {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode()
    {// This is to silence the warning
        return super.hashCode();
    }
}
