/*
 * @author Mikhail Zhigun
 * @copyright Copyright 2020, MeteoSwiss
 */
package clawfc.depscan;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

public class FortranFileBasicSummary
{
    public final List<FortranModuleBasicInfo> modules;
    public final FortranModuleBasicInfo program;

    public FortranFileBasicSummary(Map<String, LinkedHashSet<String>> moduleDependencies,
            Map<String, Integer> moduleLineStart, Map<String, Integer> moduleLineEnd,
            Map<String, LinkedHashSet<String>> programDependencies, Map<String, Integer> programLineStart,
            Map<String, Integer> programLineEnd)
    {
        ArrayList<FortranModuleBasicInfo> modules = new ArrayList<FortranModuleBasicInfo>(moduleDependencies.size());
        for (Map.Entry<String, LinkedHashSet<String>> entry : moduleDependencies.entrySet())
        {
            String moduleName = entry.getKey();
            int startLine = moduleLineStart.get(moduleName);
            int endLine = moduleLineEnd.get(moduleName);
            modules.add(new FortranModuleBasicInfo(moduleName, startLine, endLine, entry.getValue()));
        }
        this.modules = Collections.unmodifiableList(modules);
        if (!programDependencies.isEmpty())
        {
            Map.Entry<String, LinkedHashSet<String>> entry = programDependencies.entrySet().iterator().next();
            String programName = entry.getKey();
            program = new FortranModuleBasicInfo(programName, programLineStart.get(programName),
                    programLineEnd.get(programName), entry.getValue());
        } else
        {
            program = null;
        }
    }

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
