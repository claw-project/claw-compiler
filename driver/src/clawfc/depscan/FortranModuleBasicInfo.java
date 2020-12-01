/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.List;

public class FortranModuleBasicInfo
{
    final FortranStatementBasicPosition _module;
    final List<FortranStatementBasicPosition> _useModules;

    public String getName()
    {
        return getModule().getName();
    }

    public FortranStatementBasicPosition getModule()
    {
        return _module;
    }

    public List<FortranStatementBasicPosition> getUseModules()
    {
        return _useModules;
    }

    public FortranModuleBasicInfo(FortranStatementBasicPosition module, List<FortranStatementBasicPosition> useModules)
    {
        _module = module;
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
        FortranModuleBasicInfo other = (FortranModuleBasicInfo) obj;
        if (!getModule().equals(other.getModule()))
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
