/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.Collections;
import java.util.List;

public class FortranFileBasicSummary
{
    public final List<FortranModuleBasicInfo> modules;
    public final FortranModuleBasicInfo program;

    public FortranFileBasicSummary(List<clawfc.depscan.FortranModuleBasicInfo> modules,
            clawfc.depscan.FortranModuleBasicInfo program)
    {
        this.modules = Collections.unmodifiableList(modules);
        this.program = program;
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
        FortranFileBasicSummary other = (FortranFileBasicSummary) obj;
        if (!modules.equals(other.modules))
        {
            return false;
        }
        if (program == null)
        {
            if (other.program != null)
            {
                return false;
            }
        } else
        {
            if (!program.equals(other.program))
            {
                return false;
            }
        }
        return true;
    }
}
