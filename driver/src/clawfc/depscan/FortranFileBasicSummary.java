/*
 * This file is released under terms of BSD license
 * See LICENSE file for more information
 * @author Mikhail Zhigun
 */
package clawfc.depscan;

import java.util.Collections;
import java.util.List;

public class FortranFileBasicSummary
{
    public final List<FortranProgramUnitBasicInfo> units;

    public FortranFileBasicSummary(List<FortranProgramUnitBasicInfo> units)
    {
        this.units = Collections.unmodifiableList(units);
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
        if (!units.equals(other.units))
        {
            return false;
        }
        return true;
    }
}
