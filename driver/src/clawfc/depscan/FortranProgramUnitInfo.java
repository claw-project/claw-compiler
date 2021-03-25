/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import static java.lang.Math.toIntExact;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import clawfc.depscan.serial.FortranProgramUnitType;
import clawfc.depscan.serial.FortranStatementPosition;

public class FortranProgramUnitInfo
{
    clawfc.depscan.serial.FortranProgramUnitInfo _data;

    public clawfc.depscan.serial.FortranProgramUnitInfo data()
    {
        return _data;
    }

    public FortranProgramUnitType getType()
    {
        return _data.getType();
    }

    public String getName()
    {
        return _data.getInfo().getName();
    }

    public int getStartLineIdx()
    {
        return toIntExact(_data.getInfo().getStartLineIdx());
    }

    public int getEndLineIdx()
    {
        return toIntExact(_data.getInfo().getEndLineIdx());
    }

    public int getStartCharIdx()
    {
        return toIntExact(data().getInfo().getStartCharIdx());
    }

    public int getEndCharIdx()
    {
        return toIntExact(data().getInfo().getEndCharIdx());
    }

    public List<String> getUsedModuleNames()
    {
        List<String> res = new ArrayList<String>();
        for (FortranStatementPosition pos : _data.getUsedModules().getUse())
        {
            res.add(pos.getName());
        }
        return Collections.unmodifiableList(res);
    }

    public boolean getUsesClaw()
    {
        return data().isUsesClaw();
    }

    public FortranProgramUnitInfo(clawfc.depscan.serial.FortranProgramUnitInfo data)
    {
        _data = data;
    }

    public FortranProgramUnitInfo(FortranProgramUnitType type, clawfc.depscan.FortranStatementPosition pos,
            List<clawfc.depscan.FortranStatementPosition> useModules, boolean usesClaw)
    {
        _data = new clawfc.depscan.serial.FortranProgramUnitInfo();
        _data.setType(type);
        _data.setInfo(pos.getData());
        _data.setUsedModules(new clawfc.depscan.serial.FortranProgramUnitInfo.UsedModules());
        for (clawfc.depscan.FortranStatementPosition useModPos : useModules)
        {
            _data.getUsedModules().getUse().add(useModPos.getData());
        }
        _data.setUsesClaw(usesClaw);
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
        FortranProgramUnitInfo other = (FortranProgramUnitInfo) obj;
        if (!_data.getType().equals(other._data.getType()))
        {
            return false;
        }
        if (!clawfc.depscan.FortranStatementPosition.equals(_data.getInfo(), other._data.getInfo()))
        {
            return false;
        }
        if (_data.getUsedModules().getUse().size() != other._data.getUsedModules().getUse().size())
        {
            return false;
        }
        for (int i = 0, n = _data.getUsedModules().getUse().size(); i < n; ++i)
        {
            clawfc.depscan.serial.FortranStatementPosition p = _data.getUsedModules().getUse().get(i);
            clawfc.depscan.serial.FortranStatementPosition otherP = other._data.getUsedModules().getUse().get(i);
            if (!clawfc.depscan.FortranStatementPosition.equals(p, otherP))
            {
                return false;
            }
        }
        if (getStartCharIdx() != other.getStartCharIdx())
        {
            return false;
        }
        if (getEndCharIdx() != other.getEndCharIdx())
        {
            return false;
        }
        if (getUsesClaw() != other.getUsesClaw())
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
