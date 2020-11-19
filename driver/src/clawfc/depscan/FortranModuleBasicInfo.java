/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.Collection;
import java.util.List;

public class FortranModuleBasicInfo
{
    private final clawfc.depscan.serial.FortranModuleBasicInfo _data;

    public clawfc.depscan.serial.FortranModuleBasicInfo data()
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
        return _data.getUsedModules();
    }

    public FortranModuleBasicInfo(clawfc.depscan.serial.FortranModuleBasicInfo data)
    {
        _data = data;
    }

    public FortranModuleBasicInfo(String name, long startLineNum, long endLineNum, Collection<String> usedModuleNames)
    {
        _data = new clawfc.depscan.serial.FortranModuleBasicInfo();
        assign(_data, name, startLineNum, endLineNum, usedModuleNames);
    }

    public static void assign(clawfc.depscan.serial.FortranModuleBasicInfo info, String name, long startLineNum,
            long endLineNum, Collection<String> usedModuleNames)
    {
        info.setName(name);
        info.setStartLineNum(startLineNum);
        info.setEndLineNum(endLineNum);
        info.getUsedModules().addAll(usedModuleNames);
    }

    public static void assign(clawfc.depscan.serial.FortranModuleBasicInfo info,
            clawfc.depscan.serial.FortranModuleBasicInfo other)
    {
        assign(info, other.getName(), other.getStartLineNum(), other.getEndLineNum(), other.getUsedModules());
    }

    public static boolean equals(clawfc.depscan.serial.FortranModuleBasicInfo info,
            clawfc.depscan.serial.FortranModuleBasicInfo other)
    {

        if (!info.getName().equals(other.getName()))
        {
            return false;
        }
        if (info.getStartLineNum() != other.getStartLineNum())
        {
            return false;
        }
        if (info.getEndLineNum() != other.getEndLineNum())
        {
            return false;
        }
        if (!info.getUsedModules().equals(other.getUsedModules()))
        {
            return false;
        }
        return true;
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
        FortranModuleBasicInfo other = (FortranModuleBasicInfo) obj;
        return equals(this._data, other._data);
    }
}
