/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.Collection;
import java.util.List;

public class FortranModuleInfo
{
    clawfc.depscan.serial.FortranModuleInfo _data;

    public clawfc.depscan.serial.FortranModuleInfo data()
    {
        return _data;
    }

    public String getName()
    {
        return _data.getName();
    }

    // WARNING!!! startLineNum is the number of the line AFTER "module <name>"
    // statement
    public long getStartLineNum()
    {
        return _data.getStartLineNum();
    }

    // WARNING!!! endLineNum is the number of the line where "end module <name>"
    // statement is located
    public long getEndLineNum()
    {
        return _data.getEndLineNum();
    }

    public List<String> getUsedModules()
    {
        return _data.getUsedModules().getName();
    }

    public long getStartCharPos()
    {
        return data().getStartCharPos();
    }

    public long getEndCharPos()
    {
        return data().getEndCharPos();
    }

    public boolean getUsesClaw()
    {
        return data().isUsesClaw();
    }

    public FortranModuleInfo(clawfc.depscan.serial.FortranModuleInfo data)
    {
        _data = data;
    }

    public FortranModuleInfo(FortranModuleBasicInfo info, long startCharPos, long endCharPos, boolean usesCLAW)
    {
        _data = new clawfc.depscan.serial.FortranModuleInfo();
        clawfc.depscan.FortranModuleBasicInfo.assign(data(), info.data());
        data().setStartCharPos(startCharPos);
        data().setEndCharPos(endCharPos);
        data().setUsesClaw(usesCLAW);
    }

    public FortranModuleInfo(String name, long startLineNum, long endLineNum, Collection<String> usedModuleNames,
            long startCharPos, long endCharPos, boolean usesCLAW)
    {
        _data = new clawfc.depscan.serial.FortranModuleInfo();
        clawfc.depscan.FortranModuleBasicInfo.assign(data(), name, startLineNum, endLineNum, usedModuleNames);
        data().setStartCharPos(startCharPos);
        data().setEndCharPos(endCharPos);
        data().setUsesClaw(usesCLAW);
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
        FortranModuleInfo other = (FortranModuleInfo) obj;
        if (!clawfc.depscan.FortranModuleBasicInfo.equals(data(), other.data()))
        {
            return false;
        }
        if (getStartCharPos() != other.getStartCharPos())
        {
            return false;
        }
        if (getEndCharPos() != other.getEndCharPos())
        {
            return false;
        }
        if (getUsesClaw() != other.getUsesClaw())
        {
            return false;
        }
        return true;
    }
}
