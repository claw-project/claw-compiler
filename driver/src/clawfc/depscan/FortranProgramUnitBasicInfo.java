/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.List;

import clawfc.depscan.serial.FortranProgramUnitType;

public class FortranProgramUnitBasicInfo
{
    final FortranProgramUnitType _type;
    final FortranStatementBasicPosition _unit;
    final List<FortranStatementBasicPosition> _useModules;

    public FortranProgramUnitType getType()
    {
        return _type;
    }

    public String getName()
    {
        return getPosition().getName();
    }

    public FortranStatementBasicPosition getPosition()
    {
        return _unit;
    }

    public List<FortranStatementBasicPosition> getUseModules()
    {
        return _useModules;
    }

    public FortranProgramUnitBasicInfo(FortranProgramUnitType type, FortranStatementBasicPosition unit,
            List<FortranStatementBasicPosition> useModules)
    {
        _type = type;
        _unit = unit;
        _useModules = useModules;
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
        FortranProgramUnitBasicInfo other = (FortranProgramUnitBasicInfo) obj;
        if (!getType().equals(other.getType()))
        {
            return false;
        }
        if (!getPosition().equals(other.getPosition()))
        {
            return false;
        }
        if (!getUseModules().equals(other.getUseModules()))
        {
            return false;
        }
        return true;
    }
}
